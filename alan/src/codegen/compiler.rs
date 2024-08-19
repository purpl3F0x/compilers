use std::borrow::Borrow;
use std::collections::HashMap;
use std::path::Path;

use super::ast::*;
use super::scope::{Scope, Scopes};
use super::symbol_table_entries::{FunctionEntry, LValueEntry};
use super::IntType as AlanIntType;
use super::{IRError, IRResult, IRType};

use stdlib::LIBALAN_BITCODE as STDLIB_IR; // append stdlib as bitcode (see here: https://github.com/hyperledger/solang/blob/06798cd/src/emit/binary.rs#L1299)

pub use inkwell::context::Context;

use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    module::{Linkage, Module},
    passes::PassBuilderOptions,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, IntType, PointerType, VoidType},
    values::{BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue, IntValue},
    AddressSpace, IntPredicate,
};

/// Compiler module, responsible for generating LLVM IR from the AST.
/// Semantic analysis is done along the way, so the IR generation is simpler.
pub struct Compiler<'ctx> {
    // LLVM
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    target: TargetMachine,

    // Symbol Tables
    lvalue_symbol_table: Scopes<&'ctx str, LValueEntry<'ctx>>,
    function_symbol_table: Scopes<&'ctx str, FunctionEntry<'ctx>>,

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
}

impl<'ctx> Compiler<'ctx> {
    // Static methods

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
        opt_level: Option<inkwell::OptimizationLevel>,
        cpu: Option<&str>,
        features: Option<&str>,
    ) -> IRResult<TargetMachine> {
        let target_triple = TargetMachine::get_default_triple();
        // ? Add option for cross-compiling ?

        Target::initialize_native(&InitializationConfig::default())?;

        let target = Target::from_triple(&target_triple).unwrap();

        target
            .create_target_machine(
                &target_triple,
                cpu.unwrap_or("generic"),
                features.unwrap_or(""),
                opt_level.unwrap_or(inkwell::OptimizationLevel::None),
                RelocMode::PIC,
                CodeModel::Default,
            )
            .ok_or(IRError::String("Could not create target machine".to_string()))
    }

    /// Create a new compiler instance
    pub fn new(context: &'ctx Context) -> Self {
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

        let target = Self::generate_target(None, None, None).unwrap();

        Self {
            context: context,
            builder: builder,
            module: module,
            target: target,

            lvalue_symbol_table: Scopes::new(),
            function_symbol_table: Scopes::new(),

            int_type: int_type,
            char_type: char_type,
            bool_type: bool_type,
            proc_type: proc_type,
            ptr_type: ptr_type,
            const_zero: const_zero,
            int_reference_attribute: int_reference_attribute,
            char_reference_attribute: char_reference_attribute,
        }
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

    /// Compile the program into LLVM IR
    pub fn compile(&mut self, program: &'ctx FunctionAST) -> IRResult<()> {
        // ? Enchancment, allow main to have signature of (int argc, char[] argv) ?

        //* Push a new scope, this will be the outer scope of the program, holding the stdlib functions
        self.function_symbol_table.push();
        self.load_stdlib()?;

        //* Manually add the main function, to make sure ia has name of main
        if program.r_type != Type::Void {
            return Err(IRError::String("Top level function must return proc".to_string()));
        }
        if program.params.len() != 0 {
            return Err(IRError::String("Top level function must not have arguments".to_string()));
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
        self.cgen_statments(&program.body)?;

        // Hardcoded return void() function, we have already check it's a proc
        self.builder.build_return(None)?;

        self.basic_pass()
        // Ok(self.module.verify()?)
    }

    /// Runs some basic optimization passes on the module.  
    /// Will (try) to get rid of unused lambda captures(sroa -> deadargelim), and unused stdlib declarations
    fn basic_pass(&self) -> IRResult<()> {
        let pass_options = PassBuilderOptions::create();
        pass_options.set_verify_each(true);

        const PASSES: &str = "default<O0>,sroa,globaldce,deadargelim";
        self.module.run_passes(PASSES, &self.target, pass_options).map_err(|e| e.into())
    }

    pub fn set_source_file_name(&self, name: &str) {
        self.module.set_source_file_name(name);
    }

    pub fn imm_as_string(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub fn asm_as_string(&self) -> String {
        let buf: inkwell::memory_buffer::MemoryBuffer =
            self.target.write_to_memory_buffer(&self.module, inkwell::targets::FileType::Assembly).unwrap();
        String::from_utf8(buf.as_slice().to_vec()).unwrap()
    }

    pub fn imm_to_file(&self, output_file: &str) -> IRResult<()> {
        self.module.print_to_file(output_file).map_err(|e| e.into())
    }

    pub fn asm_to_file(&self, path: &Path) -> IRResult<()> {
        self.target.write_to_file(&self.module, inkwell::targets::FileType::Assembly, path).map_err(|e| e.into())
    }

    pub fn generate_binary(&self, _output_file: &str) -> IRResult<()> {
        unimplemented!("Option to generate binary isn't implemented yet!");
    }

    fn get_irtype(&self, type_: &Type) -> IRType {
        match type_ {
            Type::Int => IRType::Int,
            Type::Byte => IRType::Byte,
            Type::Void => IRType::Void,
            Type::Array(ty) => {
                // todo: this needs checking
                //todo: also we know the size of the arrays
                // todo!("arrays are not implemented yet")
                self.get_irtype(&ty).into_array_type(0)
            }
            Type::Ref(ty) => self.get_irtype(&ty).into_reference_type(),
        }
    }

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

    fn type_to_any_type(&self, ty: &Type) -> AnyTypeEnum<'ctx> {
        match ty {
            Type::Int => self.int_type.into(),
            Type::Byte => self.char_type.into(),
            Type::Void => self.proc_type.into(),
            _ => todo!(),
        }
    }

    fn type_to_basic_type(&self, ty: &Type) -> BasicTypeEnum<'ctx> {
        match ty {
            Type::Int => self.int_type.into(),
            Type::Byte => self.char_type.into(),
            Type::Void => panic!("Void type is not a basic type"), // todo: fix this, so it can return result ??
            Type::Ref(_ty) => self.ptr_type.into(),
            Type::Array(_ty) => self.ptr_type.into(),
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
                    "readString" => register_function!("readString", func_value, IRType::Reference(Box::new(IRType::Void)), vec![IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),
                    // Type conversion functions
                    "extend" => register_function!("extend", func_value, IRType::Int, vec![IRType::Byte]),
                    "shrink" => register_function!("shrink", func_value, IRType::Byte, vec![IRType::Int]),
                    // String functions
                    "strlen" => register_function!("strlen", func_value, IRType::Int, vec![IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),
                    "strcmp" => register_function!("strcmp", func_value, IRType::Int, vec![IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1))), IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),
                    "strcpy" => register_function!("strcpy", func_value, IRType::Void, vec![IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1))), IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),
                    "strcat" => register_function!("strcat", func_value, IRType::Void, vec![IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1))), IRType::Reference(Box::new(IRType::Array(Box::new(IRType::Byte), -1)))]),

                    _ => {return Err(IRError::String(format!("Unknown function in stdlib: {}", func_name_str)))}
                }
            }
        }

        Ok(())
    }

    fn cgen_literal(&mut self, literal: &'ctx Literal) -> IRResult<(IntValue<'ctx>, IRType)> {
        Ok(match literal {
            Literal::Int(i) => (self.int_type.const_int(*i as u64, true), IRType::Int),
            Literal::Byte(b) => (self.char_type.const_int(*b as u64, false), IRType::Byte),
        })
    }

    fn cgen_expresion(&mut self, expr: &'ctx ExprAST) -> IRResult<(BasicValueEnum<'ctx>, IRType)> {
        match expr {
            ExprAST::Error => Err(IRError::UnknownError),

            ExprAST::Literal(lit) => {
                let lit = self.cgen_literal(lit)?;
                Ok((lit.0.as_basic_value_enum(), lit.1))
            }

            ExprAST::InfixOp { lhs, op, rhs } => {
                let (lhs, lhs_ty) = self.cgen_expresion(lhs)?;
                let (rhs, rhs_ty) = self.cgen_expresion(rhs)?;

                let lhs = self.cgen_int_value_or_load(lhs, &lhs_ty)?;
                let rhs = self.cgen_int_value_or_load(rhs, &rhs_ty)?;

                // ? casting ??
                if lhs.get_type() != rhs.get_type() {
                    return Err(IRError::String(
                        format!("Infix operator '{}' requires both operands to have the same type, got '{}' and '{}'", op, lhs_ty, rhs_ty)
                            .to_string(),
                    ));
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
            ExprAST::PrefixOp { op, expr } => {
                let (expr, expr_ty) = self.cgen_expresion(expr)?;

                let expr = self.cgen_int_value_or_load(expr, &expr_ty)?;

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
            ExprAST::LValue(lval) => {
                let LValueEntry { ptr, ty } = self.cgen_lvalue_ptr(lval)?;
                Ok((ptr.as_basic_value_enum(), ty))
            }

            ExprAST::FunctionCall(fn_call) => {
                let (fn_value, fn_type) = self.cgen_fn_call(fn_call)?;
                if fn_type.is_void() {
                    Err(IRError::String("Function call does not return a value".to_string()))
                } else {
                    Ok((fn_value.try_as_basic_value().unwrap_left(), fn_type))
                }
            }
        }
    }

    fn cgen_lvalue_ptr(&mut self, lval: &'ctx LValueAST) -> IRResult<LValueEntry<'ctx>> {
        match lval {
            LValueAST::String(s) => {
                let str = self.builder.build_global_string_ptr(s, "glob.str")?.as_pointer_value();

                Ok(LValueEntry::new(
                    str,
                    IRType::Array(Box::new(IRType::Byte), (s.len() + 1) as i32), // ! this isn't that correct (maybe string type makes sense ?)
                ))
            }

            LValueAST::Identifier(id) => {
                let name: &str = id.borrow();
                if let Some(ptr) = self.lvalue_symbol_table.get_from_last(name) {
                    Ok(ptr.clone())
                } else {
                    Err(IRError::String(format!("undeclared undeclared '{}'", name)))
                }
            }

            LValueAST::ArraySubscript { id, expr } => {
                let (expr_res, expr_ty) = self.cgen_expresion(expr)?;
                // todo: this needs to be converted to int

                let expr_res = self.cgen_int_value_or_load(expr_res, &expr_ty)?;

                let LValueEntry { mut ptr, mut ty } =
                    self.lvalue_symbol_table.get_from_last(*id).ok_or(IRError::String(format!("undeclared undeclared '{}'", id)))?;

                // make sure we are subscripting an array
                if !ty.is_array() {
                    // no? maybe it's an array reference
                    if ty.is_reference() {
                        ty = ty.get_inner_type().unwrap().clone();
                        if !ty.is_array() {
                            return Err(IRError::String(format!("identifier '{}' is not an array, (expected array found {})", id, ty)));
                        }
                        // we must load the reference
                        ptr = self.builder.build_load(self.ptr_type, ptr, format!("deref.{}", id).as_str())?.into_pointer_value();
                    } else {
                        return Err(IRError::String(format!("identifier '{}' is not an array, (expected array found {})", id, ty)));
                    }
                }
                let iner_type: &IRType = ty.get_inner_type().unwrap();
                let pointer_ty = self.from_irtype(&ty);

                let element_pointer = match pointer_ty {
                    AnyTypeEnum::IntType(t) => unsafe {
                        self.builder.build_in_bounds_gep(t, ptr, &[self.const_zero, expr_res], format!("idx.{}", id).as_str())
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

                Ok(LValueEntry::new(element_pointer?, iner_type.clone()))
            }
        }
    }

    /// This function will get a IntValue or load an IntValue from a PointerValue (aka reference)
    fn cgen_int_value_or_load(&mut self, value: BasicValueEnum<'ctx>, ty: &IRType) -> IRResult<IntValue<'ctx>> {
        if value.is_int_value() {
            Ok(value.into_int_value())
        } else if value.is_pointer_value() {
            match ty {
                IRType::Int => Ok(self.builder.build_load(self.int_type, value.into_pointer_value(), "")?.into_int_value()),
                IRType::Byte => Ok(self.builder.build_load(self.char_type, value.into_pointer_value(), "")?.into_int_value()),
                IRType::Reference(ref ty) => {
                    if !(ty.is_int() || ty.is_byte()) {
                        return Err(IRError::String("Cannot load a non-integral type from a pointer".to_string()));
                    }
                    // 1. load the pointer
                    let ptr = self.builder.build_load(self.ptr_type, value.into_pointer_value(), "deref")?;
                    // 2. load the value from the pointer
                    self.cgen_int_value_or_load(ptr, ty)
                }
                _ => Err(IRError::String("Cannot load a non-integral type from a pointer".to_string())),
            }
        } else {
            Err(IRError::String("Expected an int or byte".to_string()))
        }
    }

    fn cgen_fn_call(&mut self, fn_call: &'ctx FnCallAST) -> IRResult<(CallSiteValue<'ctx>, IRType)> {
        let function_entry =
            self.function_symbol_table.get(fn_call.name).ok_or(IRError::String(format!("Function {} not found", fn_call.name)))?;
        //(func, func_type, func_captures)
        let func_value = &function_entry.function;
        let func_type = &function_entry.return_ty;
        let func_params = &function_entry.param_tys;
        let _func_captures = &function_entry.captures;

        let call_params = &fn_call.args;

        //* Check if the number of arguments match
        if func_params.len() != call_params.len() {
            return Err(IRError::String(format!(
                "Function '{}{}' expected {} arguments, got {}",
                fn_call.name,
                function_entry,
                func_params.len(),
                call_params.len()
            )));
        }
        //* args to be passed to builder
        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(call_params.len());

        //* Check if the types of the arguments match
        for (i, (param_ty, arg)) in func_params.iter().zip(call_params.iter()).enumerate() {
            let (mut arg, mut arg_ty) = self.cgen_expresion(arg)?;

            match &param_ty {
                //* Handle primitive types
                IRType::Int | IRType::Byte => {
                    let mut under_ty = &arg_ty; // We will use this as a placehold, if calle arg is a refernce we will use the inner type for the type checking

                    if arg_ty.is_reference() {
                        // Convert reference types to their inner type
                        under_ty = arg_ty.get_inner_type().unwrap();
                    }

                    if param_ty != under_ty {
                        return Err(IRError::String(format!(
                            "Function '{}' expected argument({}) of type {}, got {}",
                            fn_call.name, i, param_ty, arg_ty
                        )));
                    }

                    let arg = self.cgen_int_value_or_load(arg, &arg_ty)?;
                    args.push(arg.into())
                }

                //* Handle references
                IRType::Reference(_ty) => {
                    if !arg.is_pointer_value() {
                        return Err(IRError::String(format!(
                            "Cannot pass argument of type '{}' which is constant to function '{}', expected a reference - Argument should be an lvalue",
                            arg_ty, fn_call.name
                        )));
                    }
                    if arg_ty.is_reference() {
                        arg = self.builder.build_load(self.ptr_type, arg.into_pointer_value(), "deref")?;
                        arg_ty = arg_ty.get_inner_type().unwrap().clone();
                    }

                    if param_ty.get_inner_type().unwrap() != &arg_ty {
                        return Err(IRError::String(format!(
                            "Function '{}' expected argument({}) of type {}, got {}",
                            fn_call.name, i, param_ty, arg_ty
                        )));
                    }
                    args.push(arg.into())
                }
                _ => unreachable!("not implemented call"), // this should be unreachanble as we don't use pointers IRTypes, and Arrays should be references
            }
        }

        //* Pass cacptures to the function
        //* Practicallly we could save the ptr in the symbol table, when building the function
        //* But this is safer, for now
        if let Some(captures) = _func_captures {
            for (lval, _ty) in captures.into_iter() {
                let capture_ptr = self
                    .lvalue_symbol_table
                    .get_from_last(lval)
                    .ok_or_else(|| IRError::String(format!("Capture '{}' not found in the current scope", lval)))?;

                args.push(capture_ptr.ptr.into());
            }
        }

        let call = self.builder.build_call(
            func_value.clone(),
            args.as_slice(),
            format!("call.{}", func_value.get_name().to_str().expect("")).as_str(),
        )?;
        Ok((call, func_type.clone()))
    }

    fn cgen_condition(&mut self, cond: &'ctx ConditionAST) -> IRResult<IntValue<'ctx>> {
        match cond {
            ConditionAST::BoolConst(b) => Ok(self.bool_type.const_int(*b as u64, false)),

            ConditionAST::InfixLogicOp { lhs, op, rhs } => {
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

            ConditionAST::PrefixOp { op, expr } => {
                let expr = self.cgen_condition(expr)?;

                Ok(match op {
                    PrefixOperator::Not => self.builder.build_not(expr, "nottmp"),
                    _ => panic!("Should not happen"),
                }?)
            }

            ConditionAST::ExprComparison { lhs, op, rhs } => {
                let (lhs, lhs_ty) = self.cgen_expresion(lhs)?;
                let (rhs, rhs_ty) = self.cgen_expresion(rhs)?;

                let mut lhs = self.cgen_int_value_or_load(lhs, &lhs_ty)?;
                let mut rhs = self.cgen_int_value_or_load(rhs, &rhs_ty)?;

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

            ConditionAST::Error => Err(IRError::UnknownError),
        }
    }

    fn cgen_statments(&mut self, stmts: &'ctx Vec<StatementAST>) -> IRResult<()> {
        for stmt in stmts {
            self.cgen_statement(stmt)?;
            if matches!(stmt, StatementAST::Return(_)) {
                // ? add a warning if not last statment
                break;
            }
        }
        Ok(())
    }

    fn cgen_statement(&mut self, stmt: &'ctx StatementAST) -> IRResult<()> {
        match stmt {
            StatementAST::Expr(e) => {
                self.cgen_expresion(e)?;
            }

            StatementAST::Assignment { lvalue, expr } => {
                if matches!(lvalue, LValueAST::String(_)) {
                    return Err(IRError::String("Cannot assign to a string constant".to_string()));
                }
                // todo: this neeeds further checking
                let LValueEntry { ptr: mut lval_ptr, ty: mut lval_type } = self.cgen_lvalue_ptr(lvalue)?;
                let (expr, expr_type): (BasicValueEnum<'ctx>, IRType) = self.cgen_expresion(expr)?;

                if !lval_type.is_primitive() & !lval_type.is_primitive_reference() {
                    return Err(IRError::String(format!("Cannot assign to a non-integral type {}", lval_type)));
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
                        _ => return Err(IRError::String(format!("Cannot assign to a non-integral type {}", lval_type))),
                    }
                }

                let expr = self.cgen_int_value_or_load(expr, &expr_type)?;

                if expr.get_type() != self.from_irtype(&lval_type).into_int_type() {
                    return Err(IRError::String(format!(
                        "Cannot assign a value of type '{}' to a variable of type '{}'",
                        expr_type, lval_type
                    )));
                }

                self.builder.build_store(lval_ptr, expr)?;
            }

            StatementAST::FunctionCall(fn_call) => {
                let (_, fn_value) = self.cgen_fn_call(fn_call)?;
                if !fn_value.is_void() {
                    return Err(IRError::String(format!(
                        "Unused return value, when calling '{}' - that returns a {}",
                        fn_call.name, fn_value
                    )));
                }
            }

            StatementAST::Return(expr) => {
                if let Some(expr) = expr {
                    let expr_res = self.cgen_expresion(expr)?;
                    self.builder.build_return(Some(&expr_res.0))?;
                } else {
                    self.builder.build_return(None)?;
                }
            }

            StatementAST::If { condition, then, else_ } => {
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
                        return end_block.delete().map_err(|_| IRError::UnknownError);
                    };
                } else {
                    self.builder.position_at_end(end_block);
                }
            }

            StatementAST::While { condition, body } => {
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

            StatementAST::Compound(stmts) => {
                self.cgen_statments(stmts)?;
            }
            StatementAST::Error => return Err(IRError::UnknownError),
            StatementAST::Null => {}
        }

        Ok(())
    }

    fn cgen_locals(&mut self, locals: &'ctx Vec<LocalDefinitionAST<'ctx>>) -> IRResult<()> {
        for local in locals {
            // todo: check for duplicates
            // ? solved with try_insert ?
            match local {
                LocalDefinitionAST::VarDef { name, type_ } => {
                    let ty = self.get_irtype(type_);
                    let ptr = self.builder.build_alloca(self.from_irtype(&ty).into_int_type(), name)?;

                    self.lvalue_symbol_table
                        .try_insert(name, LValueEntry::new(ptr, ty))
                        .map_err(|_| IRError::String(format!(" redifinition of' {}'", name)))?;
                }
                LocalDefinitionAST::ArrayDef { name, type_, size } => {
                    let ty = self.get_irtype(type_);
                    let name = name.borrow();
                    let ir_type = ty.into_array_type(*size);
                    let size = self.int_type.const_int(*size as u64, false);

                    let ptr = self.builder.build_array_alloca(self.from_irtype(&ty).into_int_type(), size, name)?;

                    self.lvalue_symbol_table
                        .try_insert(name, LValueEntry::new(ptr, ir_type))
                        .map_err(|_| IRError::String(format!(" redifinition of' {}'", name)))?;
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
            if let LocalDefinitionAST::VarDef { name, type_: _ } = local {
                result.remove(name);
            } else if let LocalDefinitionAST::ArrayDef { name, type_: _, size: _ } = local {
                result.remove(name);
            }
        }
        result
    }

    /// Generate the IR for a function
    /// This function will create a new scope for the function, and will generate the function prototype and body
    fn cgen_function(&mut self, func: &'ctx FunctionAST<'ctx>) -> IRResult<FunctionValue<'ctx>> {
        let name = func.name;

        //* Create new scope
        self.lvalue_symbol_table.push();

        //* ------------------------ *//
        //* Create function protoype *//
        //* ------------------------ *//
        let return_type = self.type_to_any_type(&func.r_type);
        //* Build function parameters
        let mut param_types: Vec<BasicMetadataTypeEnum> = Vec::with_capacity(func.params.len());
        for param in &func.params {
            param_types.push(self.type_to_basic_type(&param.type_).into());
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

            let ir_type = self.get_irtype(&arg.type_);

            let param_ptr = self.builder.build_alloca(param.get_type(), format!("{}.addr", arg.name).as_str())?;

            self.builder.build_store(param_ptr, *param)?;

            if ir_type.is_array() {
                return Err(IRError::String(format!("Arrays can only be passed by reference")));
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
                    _ => {}
                };
            }
            // Add function signature
            param_ir_types.push(ir_type.clone());

            // Add parameter to functions scope
            self.lvalue_symbol_table
                .try_insert(arg.name, LValueEntry::new(param_ptr, ir_type))
                .map_err(|_| IRError::String(format!(" multiple arguments with name of' {}'", arg.name)))?;
        }

        //* Build captures
        let mut symbol_entry_capture_list: HashMap<&'ctx str, IRType> = HashMap::new();

        for (capture, capture_entry) in captures.iter() {
            let _capture_ptr = &capture_entry.ptr;
            let capture_ty = &capture_entry.ty;

            let (_pos, param) = func_params_iter.next().unwrap();
            param.set_name(capture);

            let mut ir_type = capture_ty.clone();

            if !ir_type.is_reference() {
                ir_type = ir_type.into_reference_type();
            }
            let param_ptr = self.builder.build_alloca(self.ptr_type, format!("{}.capture", capture).as_str())?;
            self.builder.build_store(param_ptr, *param)?;

            // todo: hint llvm ir for the dereference size
            // param_ir_types.push(ir_type.clone());
            symbol_entry_capture_list.insert(capture, ir_type.clone());
            self.lvalue_symbol_table
                .try_insert(capture, LValueEntry::new(param_ptr, ir_type))
                .map_err(|_| IRError::String("Should not happen - multiple captures with the same name".to_string()))?;
        }

        //* Insert function into functions scope
        self.function_symbol_table
            .try_insert(name, FunctionEntry::new(function, self.get_irtype(&func.r_type), param_ir_types, symbol_entry_capture_list))
            .map_err(|_| IRError::String(format!(" multiple functions with name of' {}'", name)))?;

        self.builder.position_at_end(block);

        //* Generate locals
        self.function_symbol_table.push();
        self.cgen_locals(&func.locals)?;

        //* Generte function body
        self.cgen_statments(&func.body)?;

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
                        return Err(IRError::String(format!("Control flow reaches end of non-void function '{}'", name)));
                    }
                }
            }
        }
        Ok(function)
    }
}
