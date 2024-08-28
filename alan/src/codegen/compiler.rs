use std::borrow::Borrow;
use std::collections::HashMap;
use std::path::Path;

use super::scope::{Scope, Scopes};
use super::semantic::SemanticError;
use super::symbol_table_entries::{FunctionEntry, LValueEntry};
use super::IntType as AlanIntType;
use super::{ast::*, Span};
use super::{IRError, IRResult, IRType};

use stdlib::LIBALAN_BITCODE as STDLIB_IR; // append stdlib as bitcode (see here: https://github.com/hyperledger/solang/blob/06798cd/src/emit/binary.rs#L1299)

pub use inkwell::context::Context;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    module::{Linkage, Module},
    passes::PassBuilderOptions,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, TargetTriple},
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, IntType, PointerType, VoidType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, IntValue},
    AddressSpace, IntPredicate,
};

/// Compiler module, responsible for generating LLVM IR from the AST.
/// Semantic analysis is done along the way, so the IR generation is simpler.
pub struct Compiler<'ctx> {
    //* LLVM
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    target: TargetMachine,

    //* Symbol Tables
    /// LValue symbol table, holds the variables
    lvalue_symbol_table: Scopes<&'ctx str, LValueEntry<'ctx>>,
    /// Function symbol table, holds the function definitions
    function_symbol_table: Scopes<&'ctx str, FunctionEntry<'ctx>>,
    /// The return type of the current function being compiled, update it before statment cgen, so it doesn't get overwritten by a nested function def
    current_function_return_type: IRType,

    // Types
    int_type: IntType<'ctx>,
    char_type: IntType<'ctx>,
    bool_type: IntType<'ctx>,
    proc_type: VoidType<'ctx>,
    ptr_type: PointerType<'ctx>,
    const_zero: IntValue<'ctx>,

    // ready to use llvm attributes
    int_reference_attribute: inkwell::attributes::Attribute,
    char_reference_attribute: inkwell::attributes::Attribute,
    // ? Add dereferencable generator for arrays (optional)
}

impl<'ctx> Compiler<'ctx> {
    //* ------------------------ *//
    //*      Static Methods      *//
    //* ------------------------ *//

    /// Get the version of the LLVM library as "major.minor.patch"
    pub fn llvm_version() -> String {
        let (major, minor, patch) = inkwell::support::get_llvm_version();
        format!("{}.{}.{}", major, minor, patch)
    }

    /// Get the target triple of the system f.ex. x86_64-unknown-linux-gnu
    pub fn system_triple() -> String {
        String::from_utf8_lossy(TargetMachine::get_default_triple().as_str().to_bytes()).to_string()
    }

    /// Generate a target machine with the given options
    fn generate_target(
        target_triple_option: Option<String>,
        opt_level: Option<inkwell::OptimizationLevel>,
        cpu: Option<&str>,
        features: Option<&str>,
    ) -> IRResult<TargetMachine> {
        let target_triple: TargetTriple;

        if let Some(tt) = target_triple_option {
            target_triple = TargetTriple::create(tt.as_str());
            Target::initialize_all(&InitializationConfig::default());
        } else {
            target_triple = TargetMachine::get_default_triple();
            Target::initialize_native(&InitializationConfig::default())?;
        }

        let target = Target::from_triple(&target_triple)?;

        target
            .create_target_machine(
                &target_triple,
                cpu.unwrap_or("generic"),
                features.unwrap_or(""),
                opt_level.unwrap_or(inkwell::OptimizationLevel::Default),
                RelocMode::PIC,
                CodeModel::Default,
            )
            .ok_or(IRError::String("[LLVM] Could not create target machine".to_string()))
    }

    //* ------------------------ *//
    //*      Public Methods      *//
    //* ------------------------ *//

    /// Create a new compiler instance
    pub fn new(context: &'ctx Context, target: Option<String>) -> IRResult<Self> {
        let module = context.create_module("main_module");
        let builder = context.create_builder();

        // Future proofing for different architectures
        let int_type = match std::mem::size_of::<AlanIntType>() {
            2 => context.i16_type(),
            4 => context.i32_type(),
            8 => context.i64_type(),
            16 => context.i128_type(),
            // cover any abnormal architecture
            i => context.custom_width_int_type(i as u32),
        };

        let char_type = context.i8_type();
        let proc_type = context.void_type();
        let bool_type = context.bool_type();
        let ptr_type = context.ptr_type(AddressSpace::default());
        let const_zero = int_type.const_zero();
        let attr_id = inkwell::attributes::Attribute::get_named_enum_kind_id("dereferenceable");

        let int_reference_attribute = context.create_enum_attribute(attr_id, std::mem::size_of::<AlanIntType>() as u64);
        let char_reference_attribute = context.create_enum_attribute(attr_id, 1 as u64);

        let target = Self::generate_target(target, None, None, None)?;

        Ok(Self {
            context: context,
            builder: builder,
            module: module,
            target: target,

            lvalue_symbol_table: Scopes::new(),
            function_symbol_table: Scopes::new(),
            current_function_return_type: IRType::Void,

            int_type: int_type,
            char_type: char_type,
            bool_type: bool_type,
            proc_type: proc_type,
            ptr_type: ptr_type,
            const_zero: const_zero,
            int_reference_attribute: int_reference_attribute,
            char_reference_attribute: char_reference_attribute,
        })
    }

    /// Compile the program into LLVM IR
    pub fn compile(&mut self, program: &'ctx FunctionAST) -> IRResult<()> {
        // ? Enchancment, allow main to have signature of (int argc, char[] argv) ?

        //* Push a new scope, this will be the outer scope of the program, holding the stdlib functions
        self.function_symbol_table.push();
        self.load_stdlib()?;

        //* Manually add the main function, to make sure ia has name of main
        if program.r_type.kind != TypeKind::Void {
            return Err(SemanticError::TopIsNotAProc {
                signature_span: program.signature_span,
                ret_ty: self.get_irtype(&program.r_type.kind),
                r_ty_span: program.r_type.span,
            }
            .into());
        }
        if program.params.len() != 0 {
            return Err(SemanticError::TopHasArguments { signature_span: program.signature_span }.into());
        }

        //* Start creating main
        let main_type = self.proc_type.fn_type(&[], false);
        let main_func = self.module.add_function("main", main_type, None);
        let basic_block = self.context.append_basic_block(main_func, "entry");

        self.builder.position_at_end(basic_block);

        //* Push new scope as we enter the main body now
        self.lvalue_symbol_table.push();
        self.function_symbol_table.push();

        self.cgen_locals(&program.locals)?;
        self.builder.position_at_end(basic_block);

        //* Generate function body
        self.current_function_return_type = IRType::Void;
        self.cgen_statements(&program.body)?;

        //* Check if we need to build are return (hardcoded for main-void function)
        for bb in main_func.get_basic_blocks() {
            match bb.get_terminator() {
                Some(_) => {
                    continue;
                }
                None => {
                    self.builder.build_return(None)?;
                }
            }
        }

        self.basic_pass()
        // Ok(self.module.verify()?)
    }

    //*  Private but place it here to be directly visible on compile method

    /// Runs some basic optimization passes on the module.  
    /// Will (try) to get rid of unused lambda captures(sroa -> deadargelim), and unused stdlib declarations
    fn basic_pass(&self) -> IRResult<()> {
        let pass_options = PassBuilderOptions::create();
        pass_options.set_verify_each(true);

        const PASSES: &str = "default<O0>,sroa,globaldce,deadargelim";
        self.module.run_passes(PASSES, &self.target, pass_options).map_err(|e| e.into())
    }

    /// Optimize the module using the default optimization passes
    pub fn optimize(&self) {
        let pass_options = PassBuilderOptions::create();
        pass_options.set_verify_each(true);
        pass_options.set_debug_logging(false);
        pass_options.set_loop_interleaving(true);
        pass_options.set_loop_vectorization(true);
        pass_options.set_loop_slp_vectorization(true);
        pass_options.set_loop_unrolling(true);
        pass_options.set_forget_all_scev_in_loop_unroll(true);
        pass_options.set_licm_mssa_opt_cap(1);
        pass_options.set_licm_mssa_no_acc_for_promotion_cap(10);
        pass_options.set_call_graph_profile(true);
        pass_options.set_merge_functions(true);

        // Optimize using all the passes of -O3 optimization level
        const PASSES: &str = "default<O3>";

        self.module.run_passes(PASSES, &self.target, pass_options).unwrap()
    }

    /// Set the source file name of the module
    pub fn set_source_file_name(&self, name: &str) {
        self.module.set_source_file_name(name);
    }

    /// Get the LLVM IR as a string
    pub fn imm_as_string(&self) -> String {
        self.module.print_to_string().to_string()
    }

    /// Get assembly code as a string
    pub fn asm_as_string(&self) -> String {
        let buf: inkwell::memory_buffer::MemoryBuffer =
            self.target.write_to_memory_buffer(&self.module, inkwell::targets::FileType::Assembly).unwrap();
        String::from_utf8(buf.as_slice().to_vec()).unwrap()
    }

    /// Write the LLVM IR to a file
    pub fn imm_to_file(&self, output_file: &str) -> IRResult<()> {
        self.module.print_to_file(output_file).map_err(|e| e.into())
    }

    /// Write the assembly code to a file
    pub fn asm_to_file(&self, path: &Path) -> IRResult<()> {
        self.target.write_to_file(&self.module, inkwell::targets::FileType::Assembly, path).map_err(|e| e.into())
    }

    pub fn get_target_tuple(&self) -> String {
        String::from_utf8_lossy(self.target.get_triple().as_str().to_bytes()).to_string()
    }

    pub fn generate_binary(&self, _output_file: &str) -> IRResult<()> {
        unimplemented!("Option to generate binary isn't implemented yet!");
    }

    //* ------------------------ *//
    //*     Private Methods      *//
    //* ------------------------ *//

    /// Comverts [ast::TypeKind](super::ast::TypeKind) to [IRType]
    fn get_irtype(&self, type_: &TypeKind) -> IRType {
        match type_ {
            TypeKind::Int => IRType::Int,
            TypeKind::Byte => IRType::Byte,
            TypeKind::Void => IRType::Void,
            TypeKind::Array(ty) => {
                // todo: this needs checking
                //todo: also we know the size of the arrays
                // todo!("arrays are not implemented yet")
                self.get_irtype(&ty).into_array_type(0)
            }
            TypeKind::Ref(ty) => self.get_irtype(&ty).into_reference_type(),
        }
    }

    /// Converts from [IRType] to [inkwell::types::AnyTypeEnum]
    fn from_irtype(&self, ty: &IRType) -> AnyTypeEnum<'ctx> {
        match ty {
            IRType::Int => self.int_type.into(),
            IRType::Byte => self.char_type.into(),
            IRType::Void => self.proc_type.into(),
            IRType::Array(ty, size) => {
                let inner = self.from_irtype(ty);
                if *size <= 0 {
                    self.from_irtype(ty)
                } else {
                    match inner {
                        AnyTypeEnum::IntType(t) => t.array_type(*size as u32).into(),
                        AnyTypeEnum::PointerType(t) => t.array_type(*size as u32).into(),
                        _ => todo!(),
                    }
                }
            }
            IRType::Pointer(ty) => self.from_irtype(ty),

            IRType::Reference(ty) => self.from_irtype(ty),
        }
    }

    /// Convers from [inkwell::types::AnyTypeEnum] to [ast::TypeKind](super::ast::TypeKind)
    fn type_to_any_type(&self, ty: &TypeKind) -> AnyTypeEnum<'ctx> {
        match ty {
            TypeKind::Int => self.int_type.into(),
            TypeKind::Byte => self.char_type.into(),
            TypeKind::Void => self.proc_type.into(),
            _ => todo!(),
        }
    }

    /// Converts from [ast::TypeKind](super::ast::TypeKind) to [inkwell::types::BasicTypeEnum]
    fn type_to_basic_type(&self, ty: &TypeKind) -> BasicTypeEnum<'ctx> {
        match ty {
            TypeKind::Int => self.int_type.into(),
            TypeKind::Byte => self.char_type.into(),
            TypeKind::Void => panic!("Void type is not a basic type"), // todo: fix this, so it can return result ??
            TypeKind::Ref(_ty) => self.ptr_type.into(),
            TypeKind::Array(_ty) => self.ptr_type.into(),
        }
    }

    /// Load the stdlib into the module, the stdlib symbol table is hardcoded in the stdlib
    #[rustfmt::skip]
    fn load_stdlib(&mut self) -> IRResult<()> {
        use inkwell::memory_buffer::MemoryBuffer;

        let memory = MemoryBuffer::create_from_memory_range(STDLIB_IR, "main_module");
        let external_module: Module = Module::parse_bitcode_from_buffer(&memory, self.context)?;

        macro_rules! register_function {
            ($name:expr, $func:expr, $ret_type:expr, $signature:expr) => {
                self.function_symbol_table.try_insert($name, FunctionEntry::new_extern($func, $ret_type, $signature)).unwrap()
            };
        }
        // ? Automate this ?
        // ? compile stdlib statically ?

        for function in external_module.get_functions() {
            let func_name = function.get_name();
            let func_ty = function.get_type();
            let func_name_str = func_name.to_str().unwrap();

            // Check if the function is already in the main module
            if self.module.get_function(func_name_str).is_none() {
                let func_value  = self.module.add_function(func_name_str, func_ty, None);

                // Register standard library functions based on their name
                match func_name_str {
                    // I/O functions
                    "writeInteger" => register_function!("writeInteger", func_value, IRType::Void, vec![IRType::Int]),
                    "writeByte" => register_function!("writeByte", func_value, IRType::Void, vec![IRType::Byte]),
                    "writeChar" => register_function!("writeChar", func_value, IRType::Void, vec![IRType::Byte]),
                    "writeString" => register_function!("writeString", func_value, IRType::Void, vec![IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),
                    "readInteger" => register_function!("readInteger", func_value, IRType::Int, vec![]),
                    "readByte" => register_function!("readByte", func_value, IRType::Byte, vec![]),
                    "readChar" => register_function!("readChar", func_value, IRType::Byte, vec![]),
                    "readString" => register_function!("readString", func_value, IRType::Void, vec![IRType::Int, IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),
                    // Type conversion functions
                    "extend" => register_function!("extend", func_value, IRType::Int, vec![IRType::Byte]),
                    "shrink" => register_function!("shrink", func_value, IRType::Byte, vec![IRType::Int]),
                    // String functions
                    "strlen" => register_function!("strlen", func_value, IRType::Int, vec![IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),
                    "strcmp" => register_function!("strcmp", func_value, IRType::Int, vec![IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1))), IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),
                    "strcpy" => register_function!("strcpy", func_value, IRType::Void, vec![IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1))), IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),
                    "strcat" => register_function!("strcat", func_value, IRType::Void, vec![IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1))), IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),

                    _ => {return Err(IRError::String(format!("[internal] Unknown function in stdlib: {}", func_name_str)))}
                }
            }
        }

        Ok(())
    }

    //* ------------------------ *//
    //*      Coge Generation     *//
    //* ------------------------ *//

    /// Generate IR  for a [Literal]
    fn cgen_literal(&mut self, literal: &'ctx Literal) -> IRResult<(IntValue<'ctx>, IRType)> {
        Ok(match literal {
            Literal::Int(i) => (self.int_type.const_int(*i as u64, true), IRType::Int),
            Literal::Byte(b) => (self.char_type.const_int(*b as u64, false), IRType::Byte),
        })
    }

    /// Generate IR for an [ExprAST]
    fn cgen_expresion(&mut self, expr: &'ctx ExprAST) -> IRResult<(BasicValueEnum<'ctx>, IRType)> {
        match &expr.kind {
            ExprKind::Error => Err(IRError::UnknownError), // We should never get here

            ExprKind::Literal(ref lit) => {
                let lit = self.cgen_literal(lit)?;
                Ok((lit.0.as_basic_value_enum(), lit.1))
            }

            ExprKind::InfixOp { lhs: ref lhs_expr, op, rhs: ref rhs_expr } => {
                let (lhs, lhs_ty) = self.cgen_expresion(lhs_expr)?;
                let (rhs, rhs_ty) = self.cgen_expresion(rhs_expr)?;

                let lhs = self.cgen_int_value_or_load(lhs, &lhs_ty, lhs_expr.span)?;
                let rhs = self.cgen_int_value_or_load(rhs, &rhs_ty, rhs_expr.span)?;

                // ? casting ??
                if lhs.get_type() != rhs.get_type() {
                    let lhs_decl_span = match &lhs_expr.kind {
                        ExprKind::LValue(ref lval) => match lval.kind {
                            LValueKind::Identifier(id) => Some(self.lvalue_symbol_table.get_from_last(id).unwrap().span),
                            _ => None,
                        },
                        _ => None,
                    };
                    let rhs_decl_span = match &rhs_expr.kind {
                        ExprKind::LValue(ref lval) => match lval.kind {
                            LValueKind::Identifier(id) => Some(self.lvalue_symbol_table.get_from_last(id).unwrap().span),
                            _ => None,
                        },
                        _ => None,
                    };

                    return Err(SemanticError::MissmatchExprTypes {
                        span: expr.span,
                        lhs: lhs_ty,
                        rhs: rhs_ty,
                        lhs_span: lhs_expr.span,
                        rhs_span: rhs_expr.span,
                        lhs_decl_span: lhs_decl_span,
                        rhs_decl_span: rhs_decl_span,
                    }
                    .into());
                }
                Ok((
                    match op {
                        // Will differentiate between signed and unsigned operations, and use nuw nsw to give more optimization opportunities
                        InfixOperator::Add => {
                            if rhs.get_type() == self.char_type {
                                self.builder.build_int_nuw_add(lhs, rhs, "addtmp")
                            } else {
                                self.builder.build_int_nsw_add(lhs, rhs, "addtmp")
                            }
                        }
                        InfixOperator::Sub => {
                            if rhs.get_type() == self.char_type {
                                self.builder.build_int_nuw_sub(lhs, rhs, "subtmp")
                            } else {
                                self.builder.build_int_nsw_sub(lhs, rhs, "subtmp")
                            }
                        }
                        InfixOperator::Mul => {
                            if rhs.get_type() == self.char_type {
                                self.builder.build_int_nuw_mul(lhs, rhs, "multmp")
                            } else {
                                self.builder.build_int_nsw_mul(lhs, rhs, "multmp")
                            }
                        }
                        InfixOperator::Div => {
                            if rhs.get_type() == self.char_type {
                                self.builder.build_int_unsigned_div(lhs, rhs, "divtmp")
                            } else {
                                self.builder.build_int_signed_div(lhs, rhs, "divtmp")
                            }
                        }
                        InfixOperator::Mod => {
                            if rhs.get_type() == self.char_type {
                                self.builder.build_int_unsigned_rem(lhs, rhs, "modtmp")
                            } else {
                                self.builder.build_int_signed_rem(lhs, rhs, "modtmp")
                            }
                        }
                        InfixOperator::Equal => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "eqtmp"),
                        InfixOperator::NotEqual => self.builder.build_int_compare(IntPredicate::NE, lhs, rhs, "neqtmp"),
                        InfixOperator::Greater => self.builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "gttmp"),
                        InfixOperator::Less => self.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "lttmp"),
                        InfixOperator::GreaterOrEqual => self.builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "getmp"),
                        InfixOperator::LessOrEqual => self.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "letmp"),
                        InfixOperator::LogicAnd => self.builder.build_and(lhs, rhs, "andtmp"),
                        InfixOperator::LogicOr => self.builder.build_or(lhs, rhs, "ortmp"),
                    }?
                    .as_basic_value_enum(),
                    lhs_ty, // should be the same as rhs_ty
                ))
            }
            ExprKind::PrefixOp { op, ref expr } => {
                let (expr_value, expr_ty) = self.cgen_expresion(expr)?;

                let expr = self.cgen_int_value_or_load(expr_value, &expr_ty, expr.span)?;

                Ok((
                    match op {
                        PrefixOperator::Plus => expr.as_basic_value_enum(),
                        PrefixOperator::Minus => {
                            if expr_ty.is_byte() {
                                self.builder.build_int_neg(expr, "negtmp")?.as_basic_value_enum()
                            } else {
                                self.builder.build_int_neg(expr, "negtmp")?.as_basic_value_enum()
                            }
                        }
                        PrefixOperator::Not => self.builder.build_not(expr, "nottmp")?.as_basic_value_enum(),
                    },
                    expr_ty,
                ))
            }
            ExprKind::LValue(ref lval) => {
                let LValueEntry { ptr, ty, span: _ } = self.cgen_lvalue_ptr(lval)?;
                Ok((ptr.as_basic_value_enum(), ty))
            }

            ExprKind::FunctionCall(ref fn_call) => {
                let (fn_value, fn_type) = self.cgen_fn_call(fn_call)?;

                if fn_type.is_void() {
                    let function_entry = self.function_symbol_table.get(fn_call.name).unwrap();
                    let fn_span = function_entry.span;
                    Err(SemanticError::AssignResultOfProc { span: fn_call.span, decl_span: fn_span }.into())
                } else {
                    Ok((fn_value.try_as_basic_value().unwrap_left(), fn_type))
                }
            }
        }
    }

    /// Generate IR for [LValueAST]
    fn cgen_lvalue_ptr(&mut self, lval: &'ctx LValueAST) -> IRResult<LValueEntry<'ctx>> {
        match &lval.kind {
            LValueKind::String(s) => {
                let str = self.builder.build_global_string_ptr(s, "glob.str")?.as_pointer_value();

                Ok(LValueEntry::new(
                    str,
                    IRType::Array(Box::new(IRType::Byte), (s.len() + 1) as i32), // ! this isn't that correct (maybe string type makes sense ?)
                    lval.span,
                ))
            }

            LValueKind::Identifier(id) => {
                let name: &str = id.borrow();
                if let Some(ptr) = self.lvalue_symbol_table.get_from_last(name) {
                    Ok(ptr.clone())
                } else {
                    Err(SemanticError::UndeclaredIdentifier { name: name.to_string(), span: lval.span }.into())
                }
            }

            LValueKind::ArraySubscript { id, expr } => {
                let (expr_res, expr_ty) = self.cgen_expresion(expr)?;
                // todo: this needs to be converted to int

                let expr_res = self.cgen_int_value_or_load(expr_res, &expr_ty, expr.span)?;

                let LValueEntry { mut ptr, mut ty, span } = self.lvalue_symbol_table.get_from_last(*id).ok_or_else(|| {
                    return SemanticError::UndeclaredIdentifier { name: id.to_string(), span: lval.span };
                })?;

                // make sure we are subscripting an array
                if !ty.is_array() {
                    // no? maybe it's an array reference
                    if ty.is_reference() {
                        ty = ty.get_inner_type().unwrap().clone();
                        if !ty.is_array() {
                            return Err(SemanticError::SubscriptingNonArray { span: lval.span, ty, decl_span: span }.into());
                        }
                        // we must load the reference
                        ptr = self.builder.build_load(self.ptr_type, ptr, format!("deref.{}", id).as_str())?.into_pointer_value();
                    } else {
                        return Err(SemanticError::SubscriptingNonArray { span: lval.span, ty, decl_span: span }.into());
                    }
                }
                let iner_type: &IRType = ty.get_inner_type().unwrap();
                let pointer_ty = self.from_irtype(&ty);

                let element_pointer = match pointer_ty {
                    AnyTypeEnum::IntType(t) => unsafe {
                        self.builder.build_in_bounds_gep(t, ptr, &[expr_res], format!("idx.{}", id).as_str())
                    },
                    AnyTypeEnum::ArrayType(t) => unsafe {
                        self.builder.build_in_bounds_gep(t, ptr, &[self.const_zero, expr_res], format!("idx.{}", id).as_str())
                    },
                    // ? this should be ok for now, we only have 1D arrays
                    // AnyTypeEnum::PointerType(t) => unsafe {
                    //     self.builder.build_in_bounds_gep(t, ptr, &[expr_res], format!("idx.{}", id).as_str())
                    // },
                    _ => unreachable!(),
                };

                Ok(LValueEntry::new(element_pointer?, iner_type.clone(), span))
            }
        }
    }

    /// This function will get a IntValue or load an IntValue from a PointerValue (aka reference)
    fn cgen_int_value_or_load(&mut self, value: BasicValueEnum<'ctx>, ty: &IRType, span: Span) -> IRResult<IntValue<'ctx>> {
        if value.is_int_value() {
            Ok(value.into_int_value())
        } else if value.is_pointer_value() {
            match ty {
                IRType::Int => Ok(self.builder.build_load(self.int_type, value.into_pointer_value(), "")?.into_int_value()),
                IRType::Byte => Ok(self.builder.build_load(self.char_type, value.into_pointer_value(), "")?.into_int_value()),
                IRType::Reference(ref ty) => {
                    if !(ty.is_int() || ty.is_byte()) {
                        return Err(SemanticError::PtrAsPrimitive { span: span, ty: ty.get_inner_type().unwrap().to_owned() }.into());
                    }
                    // 1. load the pointer
                    let ptr = self.builder.build_load(self.ptr_type, value.into_pointer_value(), "deref")?;
                    // 2. load the value from the pointer
                    self.cgen_int_value_or_load(ptr, ty, span)
                }
                _ => Err(SemanticError::PtrAsPrimitive { span: span, ty: ty.to_owned() }.into()),
            }
        } else {
            Err(SemanticError::PtrAsPrimitive { span: span, ty: ty.to_owned() }.into())
        }
    }

    /// Generate IR for a [FnCallAST]
    fn cgen_fn_call(&mut self, fn_call: &'ctx FnCallAST) -> IRResult<(CallSiteValue<'ctx>, IRType)> {
        let function_entry = self
            .function_symbol_table
            .get(fn_call.name)
            .ok_or(SemanticError::UndeclaredFunction { span: fn_call.span, name: fn_call.name.to_string() })?;

        let func_value = &function_entry.function;
        let func_type = &function_entry.return_ty;
        let func_params = &function_entry.param_tys;
        let func_captures = &function_entry.captures;

        let call_params = &fn_call.args;

        //* Check if the number of arguments match
        if func_params.len() != call_params.len() {
            return Err(SemanticError::MissmatchedArgumentCount {
                span: fn_call.span,
                expected: func_params.len(),
                found: call_params.len(),
                decl_span: function_entry.span,
                func_signature: function_entry.signature_string(),
            }
            .into());
        }
        //* args to be passed to builder
        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(call_params.len());

        //* Check if the types of the arguments match
        for (i, (param_ty, arg_expr)) in func_params.iter().zip(call_params.iter()).enumerate() {
            let (mut arg, mut arg_ty) = self.cgen_expresion(arg_expr)?;

            match &param_ty {
                //* Handle primitive types
                IRType::Int | IRType::Byte => {
                    let mut under_ty = &arg_ty; // We will use this as a placehold, if calle arg is a refernce we will use the inner type for the type checking

                    if arg_ty.is_reference() {
                        // Convert reference types to their inner type
                        under_ty = arg_ty.get_inner_type().unwrap();
                    }

                    if param_ty != under_ty {
                        return Err(SemanticError::ArgumentTypeMismatch {
                            span: arg_expr.span,
                            arg_index: i,
                            expected: param_ty.clone(),
                            found: arg_ty.clone(),
                            decl_span: function_entry.span,
                            func_signature: function_entry.signature_string(),
                        }
                        .into());
                    }

                    let arg = self.cgen_int_value_or_load(arg, &arg_ty, arg_expr.span)?;
                    args.push(arg.into())
                }

                //* Handle references
                IRType::Reference(_ty) => {
                    if !arg.is_pointer_value() {
                        return Err(SemanticError::ConstantAsRef {
                            span: arg_expr.span,
                            ty: arg_ty,
                            ref_ty: param_ty.clone(),
                            decl_span: function_entry.span,
                        }
                        .into());
                    }
                    if arg_ty.is_reference() {
                        arg = self.builder.build_load(self.ptr_type, arg.into_pointer_value(), "deref")?;
                        arg_ty = arg_ty.get_inner_type().unwrap().clone();
                    }

                    if param_ty.get_inner_type().unwrap() != &arg_ty {
                        return Err(SemanticError::ArgumentTypeMismatch {
                            span: arg_expr.span,
                            arg_index: i,
                            expected: param_ty.clone(),
                            found: arg_ty.clone(),
                            decl_span: function_entry.span,
                            func_signature: function_entry.signature_string(),
                        }
                        .into());
                    }
                    args.push(arg.into())
                }
                _ => unreachable!("not implemented call"), // this should be unreachanble as we don't use pointers IRTypes, and Arrays should be references
            }
        }

        //* Pass cacptures to the function
        //* Practically we could save the ptr in the symbol table, when building the function
        //* But this is safer, and prefered for now
        if let Some(captures) = func_captures {
            for (lval, _lval_ty) in captures.into_iter() {
                // ! this needs some furter checking for arrays and nested references
                let capture_entry = self
                    .lvalue_symbol_table
                    .get_from_last(lval)
                    .ok_or_else(|| IRError::String(format!("[Internal Error] Capture '{}' not found in the current scope", lval)))?; // todo: transform to internal error

                if capture_entry.ty.is_reference() {
                    let capture_ptr = self.builder.build_load(self.ptr_type, capture_entry.ptr, "deref")?;
                    args.push(capture_ptr.into());
                } else {
                    args.push(capture_entry.ptr.into());
                }

                // let arg = capture_entry.ptr;
                // args.push(arg.into());
            }
        }

        let call = self.builder.build_call(
            func_value.clone(),
            args.as_slice(),
            format!("call.{}", func_value.get_name().to_str().expect("")).as_str(),
        )?;
        Ok((call, func_type.clone()))
    }

    /// Generate IR for a [ConditionAST]
    fn cgen_condition(&mut self, cond: &'ctx ConditionAST) -> IRResult<IntValue<'ctx>> {
        match &cond.kind {
            ConditionKind::BoolConst(ref b) => Ok(self.bool_type.const_int(*b as u64, false)),

            ConditionKind::InfixLogicOp { ref lhs, op, ref rhs } => {
                let lhs = self.cgen_condition(lhs)?;
                let rhs = self.cgen_condition(rhs)?;

                Ok(match op {
                    InfixOperator::Equal => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "eqtmp"),
                    InfixOperator::NotEqual => self.builder.build_int_compare(IntPredicate::NE, lhs, rhs, "neqtmp"),
                    InfixOperator::Greater => self.builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "gttmp"),
                    InfixOperator::Less => self.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "lttmp"),
                    InfixOperator::GreaterOrEqual => self.builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "getmp"),
                    InfixOperator::LessOrEqual => self.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "letmp"),
                    InfixOperator::LogicAnd => self.builder.build_and(lhs, rhs, "andtmp"),
                    InfixOperator::LogicOr => self.builder.build_or(lhs, rhs, "ortmp"),
                    _ => panic!("Should not happen"),
                }?)
            }

            ConditionKind::PrefixOp { op, ref expr } => {
                let expr = self.cgen_condition(expr)?;

                Ok(match op {
                    PrefixOperator::Not => self.builder.build_not(expr, "nottmp"),
                    _ => panic!("Should not happen"),
                }?)
            }

            ConditionKind::ExprComparison { lhs: ref lhs_expr, op, rhs: ref rhs_expr } => {
                let (lhs, lhs_ty) = self.cgen_expresion(lhs_expr)?;
                let (rhs, rhs_ty) = self.cgen_expresion(rhs_expr)?;

                let mut lhs = self.cgen_int_value_or_load(lhs, &lhs_ty, lhs_expr.span)?;
                let mut rhs = self.cgen_int_value_or_load(rhs, &rhs_ty, rhs_expr.span)?;

                // Enable char-int comparisons
                if rhs.get_type() != lhs.get_type() {
                    if rhs.get_type() == self.char_type {
                        rhs = self.builder.build_int_z_extend(rhs, self.int_type, "char_to_int")?;
                    } else {
                        lhs = self.builder.build_int_z_extend(lhs, self.int_type, "char_to_int")?;
                    }
                }

                Ok(match op {
                    InfixOperator::Equal => self.builder.build_int_compare(IntPredicate::EQ, lhs, rhs, "eqtmp"),
                    InfixOperator::NotEqual => self.builder.build_int_compare(IntPredicate::NE, lhs, rhs, "neqtmp"),
                    InfixOperator::Greater => self.builder.build_int_compare(IntPredicate::SGT, lhs, rhs, "gttmp"),
                    InfixOperator::Less => self.builder.build_int_compare(IntPredicate::SLT, lhs, rhs, "lttmp"),
                    InfixOperator::GreaterOrEqual => self.builder.build_int_compare(IntPredicate::SGE, lhs, rhs, "getmp"),
                    InfixOperator::LessOrEqual => self.builder.build_int_compare(IntPredicate::SLE, lhs, rhs, "letmp"),
                    _ => panic!("Should not happen"),
                }?)
            }

            ConditionKind::Error => Err(IRError::UnknownError),
        }
    }

    fn cgen_statements(&mut self, stmts: &'ctx Vec<StatementAST>) -> IRResult<()> {
        for stmt in stmts {
            self.cgen_statement(stmt)?;
            if matches!(stmt.kind, StatementKind::Return(_)) {
                // ? add a warning if not last statment
                break;
            }
        }
        Ok(())
    }

    /// Generate IR for a [StatementAST]
    fn cgen_statement(&mut self, stmt: &'ctx StatementAST) -> IRResult<()> {
        match &stmt.kind {
            StatementKind::Expr(e) => {
                self.cgen_expresion(e)?;
            }

            StatementKind::Assignment { lvalue, expr } => {
                if matches!(lvalue.kind, LValueKind::String(_)) {
                    return Err(SemanticError::AssignToString { span: stmt.span }.into());
                }
                // todo: this neeeds further checking
                let LValueEntry { ptr: mut lval_ptr, ty: mut lval_type, span } = self.cgen_lvalue_ptr(lvalue)?;
                let (expr_value, expr_type): (BasicValueEnum<'ctx>, IRType) = self.cgen_expresion(expr)?;

                if !lval_type.is_primitive() & !lval_type.is_primitive_reference() {
                    return Err(SemanticError::AssignTypeMismatch {
                        span: stmt.span,
                        expected: lval_type,
                        found: expr_type,
                        decl_span: span,
                    }
                    .into());
                }

                if lval_type.is_reference() {
                    lval_type = lval_type.get_inner_type().unwrap().clone();
                    match lval_type {
                        IRType::Int => {
                            lval_ptr = self.builder.build_load(self.ptr_type, lval_ptr, "deref")?.into_pointer_value();
                        }
                        IRType::Byte => {
                            lval_ptr = self.builder.build_load(self.ptr_type, lval_ptr, "deref")?.into_pointer_value();
                        }
                        _ => {
                            return Err(SemanticError::AssignTypeMismatch {
                                span: stmt.span,
                                expected: lval_type,
                                found: expr_type,
                                decl_span: span,
                            }
                            .into())
                        }
                    }
                }

                let expr = self.cgen_int_value_or_load(expr_value, &expr_type, expr.span)?;

                if expr.get_type() != self.from_irtype(&lval_type).into_int_type() {
                    return Err(SemanticError::AssignTypeMismatch {
                        span: stmt.span,
                        expected: lval_type,
                        found: expr_type,
                        decl_span: span,
                    }
                    .into());
                }

                self.builder.build_store(lval_ptr, expr)?;
            }

            StatementKind::FunctionCall(fn_call) => {
                let (_, fn_value) = self.cgen_fn_call(fn_call)?;

                if !fn_value.is_void() {
                    // According to the spec, this is a hard-error, not a warning
                    // ? maybe introduce a maybe_unused attribute ?
                    let fn_entry = self.function_symbol_table.get(fn_call.name).unwrap();

                    return Err(SemanticError::UnusedReturnValue {
                        span: stmt.span,
                        decl_span: fn_entry.span,
                        ty: fn_value,
                        ret_ty_span: fn_entry.ret_ty_span,
                    }
                    .into());
                }
            }

            StatementKind::Return(expr) => {
                if let Some(expr) = expr {
                    let (expr_res, expr_ty) = self.cgen_expresion(expr)?;
                    // todo: add func delcaration to the error (see also bellow)
                    if expr_ty != self.current_function_return_type {
                        return Err(SemanticError::MissmatchedReturnType {
                            span: expr.span,
                            expected: self.current_function_return_type.clone(),
                            found: expr_ty,
                        }
                        .into());
                    }

                    let res = self.cgen_int_value_or_load(expr_res, &expr_ty, expr.span)?;
                    self.builder.build_return(Some(&res))?;
                } else {
                    if self.current_function_return_type.is_void() {
                        self.builder.build_return(None)?;
                    } else {
                        // todo: add func delcaration to the error
                        return Err(SemanticError::MissmatchedReturnType {
                            span: stmt.span,
                            expected: self.current_function_return_type.clone(),
                            found: IRType::Void,
                        }
                        .into());
                    }
                }
            }

            StatementKind::If { condition, then, else_ } => {
                // todo: chain if else, instead of recurscivly generating the statments to avoid multiple branches
                let block = self.builder.get_insert_block().unwrap();
                let current_function = block.get_parent().unwrap();

                let cond = self.cgen_condition(condition)?;

                let then_block = self.context.append_basic_block(current_function, "if.then");
                let end_block: BasicBlock = self.context.append_basic_block(current_function, "if.end");

                let else_block = match else_ {
                    Some(_) => Some(self.context.append_basic_block(current_function, "if.else")),
                    None => None,
                };

                self.builder.build_conditional_branch(cond, then_block, else_block.unwrap_or(end_block))?;

                //* Build then block
                self.builder.position_at_end(then_block);
                self.cgen_statement(then)?;
                if then_block.get_terminator().is_none() {
                    self.builder.build_unconditional_branch(end_block)?;
                }

                //* Build(?) else block
                if let Some(else_block) = else_block {
                    self.builder.position_at_end(else_block);
                    self.cgen_statement(else_.as_ref().unwrap())?;
                    // build unconditional branch to end block, if no return statement
                    if else_block.get_terminator().is_none() {
                        self.builder.build_unconditional_branch(end_block)?;
                    }
                }
                //* End block
                if end_block.get_first_use() == None {
                    // if both branches return, delete end block
                    unsafe {
                        return end_block.delete().map_err(|_| IRError::String("[LLVM] Failed to delete block".to_string()));
                    };
                } else {
                    self.builder.position_at_end(end_block);
                }
            }

            StatementKind::While { condition, body } => {
                let block = self.builder.get_insert_block().unwrap();
                let current_function = block.get_parent().unwrap();

                let while_cond = self.context.append_basic_block(current_function, "while.cond");
                let while_body = self.context.append_basic_block(current_function, "while.body");
                let while_end = self.context.append_basic_block(current_function, "while.end");

                // Build condition block

                self.builder.build_unconditional_branch(while_cond)?;
                self.builder.position_at_end(while_cond);

                let cond = self.cgen_condition(condition)?;
                self.builder.build_conditional_branch(cond, while_body, while_end)?;

                // Build body block
                self.builder.position_at_end(while_body);
                self.cgen_statement(body)?;

                self.builder.build_unconditional_branch(while_cond)?;

                // Build end block
                self.builder.position_at_end(while_end);
            }

            StatementKind::Compound(stmts) => {
                self.cgen_statements(stmts)?;
            }
            StatementKind::Error => return Err(IRError::UnknownError),
        }

        Ok(())
    }

    /// Generate IR for a [LocalDefinitionAST]
    /// Definition will be added in order of appearance
    fn cgen_locals(&mut self, locals: &'ctx Vec<LocalDefinitionAST<'ctx>>) -> IRResult<()> {
        for local in locals {
            match local {
                LocalDefinitionAST::VarDef(VarDefAST { name, type_, span }) => {
                    let ty = self.get_irtype(&type_.kind);
                    let ptr = self.builder.build_alloca(self.from_irtype(&ty).into_int_type(), name)?;

                    self.lvalue_symbol_table.try_insert(name, LValueEntry::new(ptr, ty, *span)).map_err(|e| {
                        SemanticError::RedeclaringIdentifier { span: *span, name: name.to_string(), decl_span: e.entry.span }
                    })?;
                }
                LocalDefinitionAST::ArrayDef(ArrayDefAST { name, type_, size, span }) => {
                    let ty = self.get_irtype(&type_.kind);
                    let name = name.borrow();
                    let ir_type = ty.into_array_type(*size);
                    let size = self.int_type.const_int(*size as u64, false);

                    let ptr = self.builder.build_array_alloca(self.from_irtype(&ty).into_int_type(), size, name)?;

                    self.lvalue_symbol_table.try_insert(name, LValueEntry::new(ptr, ir_type, *span)).map_err(|e| {
                        SemanticError::RedeclaringIdentifier { span: *span, name: name.to_string(), decl_span: e.entry.span }
                    })?;
                }

                LocalDefinitionAST::FunctionDef(f) => {
                    // save current block (aka position)
                    let current_block = self.builder.get_insert_block().unwrap();

                    self.cgen_function(f)?;

                    // restore position
                    self.builder.position_at_end(current_block);
                }
            }
        }
        Ok(())
    }

    /// Helper method to generate the captures for a function.  
    /// Copies tha outer scope's symbol table and removes the names that are shadowed by the function parameters and locals.  
    ///
    /// Nested scopes are implemented using λ lifting..
    /// This function will get the outer scope of function and will effectively generate a hashmap of variables
    /// that will need to be passed to the function as parameters
    /// The goal is to transform nested functions into top-level functions by converting local variables they access into explicit parameters.
    /// This eliminates the need for nested scope management. See also internals of [cgen_function](Self::cgen_function) and [cgen_fn_call](Self::cgen_fn_call)
    fn generate_captures(
        &self,
        params: &Vec<VarDefAST<'ctx>>,
        locals: &Vec<LocalDefinitionAST<'ctx>>,
    ) -> Scope<&'ctx str, LValueEntry<'ctx>> {
        let captures = self.lvalue_symbol_table.get_upper_scope().unwrap();
        let mut result = captures.clone();

        for param in params {
            result.remove(&param.name);
        }
        for local in locals {
            if let LocalDefinitionAST::VarDef(VarDefAST { name, type_: _, span: _ }) = local {
                result.remove(name);
            } else if let LocalDefinitionAST::ArrayDef(ArrayDefAST { name, type_: _, size: _, span: _ }) = local {
                result.remove(name);
            }
        }
        result
    }

    /// Generate the IR for a [FunctionAST]
    /// This function will create a new scope for the function, and will generate the function prototype and body
    fn cgen_function(&mut self, func: &'ctx FunctionAST<'ctx>) -> IRResult<FunctionValue<'ctx>> {
        let name = func.name;

        //* Create new scope
        self.lvalue_symbol_table.push();

        //* ------------------------ *//
        //* Create function protoype *//
        //* ------------------------ *//
        let return_type = self.type_to_any_type(&func.r_type.kind);
        //* Build function parameters
        let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(func.params.len());
        for param in &func.params {
            param_types.push(self.type_to_basic_type(&param.type_.kind).into());
        }
        // In addition to the parameters, we need to pass the captures

        let captures = self.generate_captures(&func.params, &func.locals);

        // add a reference to the functions signature for each capture
        for _capture in captures.iter() {
            param_types.push(self.ptr_type.into());
        }
        //* Build return type
        let func_type: inkwell::types::FunctionType = match return_type {
            AnyTypeEnum::VoidType(_) => self.proc_type.fn_type(param_types.as_slice(), false),
            AnyTypeEnum::IntType(t) => t.fn_type(param_types.as_slice(), false),
            _ => unreachable!(),
        };

        let function = self.module.add_function(name, func_type, Some(Linkage::Internal));

        //* Create entry block
        let block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(block);

        //* ------------------------ *//
        //*     Build parameters     *//
        //* ------------------------ *//
        let mut param_ir_types: Vec<IRType> = Vec::with_capacity(func.params.len() + captures.len());

        let func_params = function.get_params();
        let mut func_params_iter = func_params.iter().enumerate();

        //* Build normal parameters
        for arg in func.params.iter() {
            let (pos, param) = func_params_iter.next().unwrap();

            param.set_name(arg.name);

            let ir_type = self.get_irtype(&arg.type_.kind);

            let param_ptr = self.builder.build_alloca(param.get_type(), format!("{}.addr", arg.name).as_str())?;

            self.builder.build_store(param_ptr, *param)?;

            if ir_type.is_array() {
                return Err(SemanticError::ArrayAsNonRef { span: arg.span, ty: ir_type, decl_span: func.signature_span }.into());
            }

            // Hint llvm-ir that we have a reference
            if ir_type.is_reference() {
                match ir_type.get_inner_type().unwrap() {
                    IRType::Int => {
                        function.add_attribute(inkwell::attributes::AttributeLoc::Param(pos as u32), self.int_reference_attribute)
                    }
                    IRType::Byte => {
                        function.add_attribute(inkwell::attributes::AttributeLoc::Param(pos as u32), self.char_reference_attribute)
                    }
                    // todo: Add array deref attribute generator for arrays
                    _ => {}
                };
            }
            // Add function signature
            param_ir_types.push(ir_type.clone());

            // Add parameter to functions scope
            self.lvalue_symbol_table.try_insert(arg.name, LValueEntry::new(param_ptr, ir_type, arg.span)).map_err(|e| {
                SemanticError::MultipleArgumentsWithSameName {
                    span: func.signature_span,
                    name: arg.name.to_owned(),
                    span1: e.entry.span,
                    span2: arg.span,
                }
            })?;
        }

        //* Build captures
        let mut symbol_entry_capture_list: HashMap<&'ctx str, IRType> = HashMap::new();

        for (capture, capture_entry) in captures.iter() {
            let _capture_ptr = &capture_entry.ptr;
            let capture_ty = &capture_entry.ty;

            let (pos, param) = func_params_iter.next().unwrap();
            param.set_name(capture);
            let mut ir_type = capture_ty.clone();
            if !ir_type.is_reference() {
                ir_type = ir_type.into_reference_type();
            }
            let param_ptr = self.builder.build_alloca(self.ptr_type, format!("{}.capture", capture).as_str())?;
            self.builder.build_store(param_ptr, *param)?;

            // Hint llvm-ir that we have a reference
            if ir_type.is_reference() {
                match ir_type.get_inner_type().unwrap() {
                    IRType::Int => {
                        function.add_attribute(inkwell::attributes::AttributeLoc::Param(pos as u32), self.int_reference_attribute)
                    }
                    IRType::Byte => {
                        function.add_attribute(inkwell::attributes::AttributeLoc::Param(pos as u32), self.char_reference_attribute)
                    }
                    // todo: Add array deref attribute generator for arrays
                    _ => {}
                };
            }

            symbol_entry_capture_list.insert(capture, ir_type.clone());
            self.lvalue_symbol_table.try_insert(capture, LValueEntry::new(param_ptr, ir_type, capture_entry.span)).map_err(|_| {
                IRError::String("[Internal Error] This shouldn't have happed - multiple captures with the same name".to_string())
            })?;
        }

        //* Insert function into functions scope
        self.function_symbol_table
            .try_insert(
                name,
                FunctionEntry::new(
                    function,
                    self.get_irtype(&func.r_type.kind),
                    param_ir_types,
                    symbol_entry_capture_list,
                    func.signature_span,
                    func.r_type.span,
                ),
            )
            .map_err(|e| SemanticError::RedeclaringFunction {
                span: func.signature_span,
                name: name.to_string(),
                decl_span: e.entry.span,
            })?;

        self.builder.position_at_end(block);

        //* Generate locals
        self.function_symbol_table.push();
        self.cgen_locals(&func.locals)?;

        //* Generte function body
        self.current_function_return_type = self.get_irtype(&func.r_type.kind);
        self.cgen_statements(&func.body)?;

        //* Prepare to leave function
        self.lvalue_symbol_table.pop();
        self.function_symbol_table.pop();

        //* Check if any module of the CFG returns, and build return for void functions
        for bb in function.get_basic_blocks() {
            match bb.get_terminator() {
                Some(_) => {
                    continue;
                }
                None => {
                    if return_type.is_void_type() {
                        self.builder.position_at_end(bb);
                        self.builder.build_return(None)?;
                    } else {
                        return Err(SemanticError::ReachEndOfNonVoidFunction {
                            span: func.span,
                            ret_ty: self.get_irtype(&func.r_type.kind),
                        }
                        .into());
                    }
                }
            }
        }
        Ok(function)
    }
}
