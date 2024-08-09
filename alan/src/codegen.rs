use core::fmt;
use std::any::Any;
use std::borrow::{Borrow, BorrowMut};
use std::f64::consts::E;
use std::hash::Hash;
use std::path::{Path, PathBuf};

use super::ast::*;
use super::IntType as AlanIntType;

use inkwell::builder::BuilderError;
pub use inkwell::context::Context;

use inkwell::support::LLVMString;
use inkwell::types::{AnyType, AnyTypeEnum};
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::values::GlobalValue;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::{self},
    module::Module,
    passes::PassBuilderOptions,
    passes::PassManager,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum, IntType, PointerType, VoidType},
    values::{AsValueRef, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
    OptimizationLevel::Aggressive,
};

use llvm::LLVMValue;
use llvm_sys::{self as llvm, target};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub enum IRError
{
    UnknownError,
    BuilderError(BuilderError),
    String(String),
}

pub type IRResult<T> = Result<T, IRError>;

impl From<BuilderError> for IRError
{
    fn from(error: BuilderError) -> Self
    {
        IRError::BuilderError(error)
    }
}

impl From<LLVMString> for IRError
{
    fn from(error: LLVMString) -> Self
    {
        IRError::String(error.to_string())
    }
}

impl std::fmt::Display for IRError
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result
    {
        match self {
            IRError::UnknownError => write!(f, "Unknown error"),
            IRError::BuilderError(e) => write!(f, "Builder error: {}", e),
            IRError::String(s) => write!(f, "{}", s),
        }
    }
}

pub struct Compiler<'ctx>
{
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    // hash map of the addresses of variabels and their underlying types
    variables: HashMap<&'ctx str, (PointerValue<'ctx>, AnyTypeEnum<'ctx>)>,

    int_type: IntType<'ctx>,
    char_type: IntType<'ctx>,
    int_ptr_type: PointerType<'ctx>, // ! llvm-18 uses opaque pointers, there no point doing this fix
    char_ptr_type: PointerType<'ctx>,
    proc_type: VoidType<'ctx>,
    const_zero: IntValue<'ctx>,
}

impl<'ctx> Compiler<'ctx>
{
    pub fn new(context: &'ctx Context) -> Self
    {
        let module = context.create_module("main_module");
        let builder = context.create_builder();
        let variables = HashMap::new();

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
        let int_ptr_type = context.ptr_type(AddressSpace::default());
        let char_ptr_type = context.ptr_type(AddressSpace::default());
        let proc_type = context.void_type();

        let const_zero = int_type.const_zero();

        Self {
            context: context,
            builder: builder,
            module: module,
            variables: variables,
            int_type: int_type,
            char_type: char_type,
            int_ptr_type: int_ptr_type,
            char_ptr_type: char_ptr_type,
            proc_type: proc_type,
            const_zero: const_zero,
        }
    }

    pub fn optimize(&self)
    {
        let target_machine = self.generate_target(None, None, None).unwrap();

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

        self.module
            .run_passes(PASSES, &target_machine, pass_options)
            .unwrap()
    }

    pub fn compile(&mut self, program: &'ctx FunctionAST) -> IRResult<()>
    {
        // ? Enchancment, allow main to have signature of (int argc, char[] argv) ?

        // TODO: add the stdlib functions
        self.load_stdlib();

        // Manually add the main function, to make sure ia has name of main

        let main_type = self.proc_type.fn_type(&[], false);
        let main_func = self.module.add_function("main", main_type, None);
        let basic_block = self.context.append_basic_block(main_func, "entry");

        self.builder.position_at_end(basic_block);

        for local in &program.locals {
            match local {
                LocalDefinitionAST::VarDef { name, type_ } => {
                    let type_ = self.get_type(type_);
                    let ptr = self.builder.build_alloca(type_.into_int_type(), name);

                    self.variables.insert(name, (ptr.unwrap(), type_));
                }
                LocalDefinitionAST::ArrayDef { name, type_, size } => {
                    let ty = self.get_type(type_);
                    let name = name.borrow();
                    let size = self.int_type.const_int(*size as u64, false);

                    let ptr = self
                        .builder
                        .build_array_alloca(ty.into_int_type(), size, name);

                    self.variables.insert(name, (ptr.unwrap(), ty.clone()));
                }
                LocalDefinitionAST::FunctionDef(_) => todo!("FunctionDef"),
            }
        }

        // Generate function body
        for stmt in &program.body {
            self.cgen_statement(stmt)?;
        }

        self.builder.position_at_end(basic_block);

        // Hardcoded return void() function, should have passed semantic
        let _ = self.builder.build_return(None);

        Ok(self.module.verify()?)
    }

    pub fn set_source_file_name(&self, name: &str)
    {
        self.module.set_source_file_name(name);
    }

    pub fn imm_as_string(&self) -> String
    {
        self.module.print_to_string().to_string()
    }

    pub fn asm_as_string(&self) -> String
    {
        let target_machine = self.generate_target(None, None, None).unwrap();

        let buf: inkwell::memory_buffer::MemoryBuffer = target_machine
            .write_to_memory_buffer(&self.module, inkwell::targets::FileType::Assembly)
            .unwrap();

        String::from_utf8(buf.as_slice().to_vec()).unwrap()
    }

    pub fn generate_binary(&self, _output_file: &str) -> IRResult<()>
    {
        unimplemented!("Option to generate binary isn't implemented yet!");
    }

    fn get_type(&self, type_: &Type) -> AnyTypeEnum<'ctx>
    {
        match type_ {
            Type::Int => self.int_type.into(),
            Type::Byte => self.char_type.into(),
            Type::Ref(t) => self.get_type(t),

            Type::Void => self.proc_type.into(),
            Type::Array(t) => {
                // todo: this needs checking
                //todo: also we know the size of the arrays
                let elem_type = self.get_type(t);
                let array_type = elem_type.into_array_type();
                array_type.into()
            }
        }
    }

    fn generate_target(
        &self,
        opt_level: Option<inkwell::OptimizationLevel>,
        cpu: Option<&str>,
        features: Option<&str>,
    ) -> IRResult<TargetMachine>
    {
        let target_triple = TargetMachine::get_default_triple();
        Target::initialize_all(&InitializationConfig::default());

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
            .ok_or(IRError::String(
                "Could not create target machine".to_string(),
            ))
    }

    fn load_stdlib(&mut self)
    {
        // todo: add the stdlib functions !!!
        use inkwell::memory_buffer::MemoryBuffer;
        let memory = MemoryBuffer::create_from_memory_range(STDLIB_IR, "main_module");

        let external_module = Module::parse_bitcode_from_buffer(&memory, self.context)
            .expect("Failed to parse the bitcode");

        // Manually add external declarations to the main module
        for function in external_module.get_functions() {
            if self
                .module
                .get_function(function.get_name().to_str().unwrap())
                .is_none()
            {
                self.module.add_function(
                    function.get_name().to_str().unwrap(),
                    function.get_type(),
                    None,
                );
            }
        }
    }

    fn cgen_literal(&mut self, literal: &'ctx Literal) -> IRResult<IntValue<'ctx>>
    {
        Ok(match literal {
            Literal::Int(i) => self.int_type.const_int(*i as u64, true),
            Literal::Byte(b) => self.char_type.const_int(*b as u64, false),
        })
    }

    /// Compiles the specified `Expr` into an LLVM `IntValue`.
    fn cgen_expresion(&mut self, expr: &'ctx ExprAST) -> IRResult<BasicValueEnum<'ctx>>
    {
        match expr {
            ExprAST::Error => Err(IRError::UnknownError),
            ExprAST::Literal(lit) => Ok(self.cgen_literal(lit)?.as_basic_value_enum()),
            ExprAST::InfixOp { lhs, op, rhs } => {
                let lhs = self.cgen_expresion(lhs)?.into_int_value();
                let rhs = self.cgen_expresion(rhs)?.into_int_value();

                if lhs.get_type() != rhs.get_type() {
                    return Err(IRError::String(
                        format!(
                            "Infix operator {:?} requires both operands to have the same type, got {} and {}",
                            op, lhs.get_type(), rhs.get_type()
                        ).to_string()
                    ));
                }

                Ok(match op {
                    InfixOperator::Add => self.builder.build_int_add(lhs, rhs, "addtmp"),
                    InfixOperator::Sub => self.builder.build_int_sub(lhs, rhs, "subtmp"),
                    InfixOperator::Mul => self.builder.build_int_mul(lhs, rhs, "multmp"),
                    InfixOperator::Div => self.builder.build_int_signed_div(lhs, rhs, "divtmp"),
                    InfixOperator::Mod => self.builder.build_int_signed_rem(lhs, rhs, "modtmp"),
                    InfixOperator::Equal => {
                        self.builder
                            .build_int_compare(IntPredicate::EQ, lhs, rhs, "eqtmp")
                    }
                    InfixOperator::NotEqual => {
                        self.builder
                            .build_int_compare(IntPredicate::NE, lhs, rhs, "neqtmp")
                    }
                    InfixOperator::Greater => {
                        self.builder
                            .build_int_compare(IntPredicate::SGT, lhs, rhs, "gttmp")
                    }
                    InfixOperator::Less => {
                        self.builder
                            .build_int_compare(IntPredicate::SLT, lhs, rhs, "lttmp")
                    }
                    InfixOperator::GreaterOrEqual => {
                        self.builder
                            .build_int_compare(IntPredicate::SGE, lhs, rhs, "getmp")
                    }
                    InfixOperator::LessOrEqual => {
                        self.builder
                            .build_int_compare(IntPredicate::SLE, lhs, rhs, "letmp")
                    }
                    InfixOperator::LogicAnd => self.builder.build_and(lhs, rhs, "andtmp"),
                    InfixOperator::LogicOr => self.builder.build_or(lhs, rhs, "ortmp"),
                }?
                .as_basic_value_enum())
            }
            ExprAST::PrefixOp { op, expr } => {
                let expr = self.cgen_expresion(expr)?.into_int_value();

                Ok(match op {
                    PrefixOperator::Plus => expr.as_basic_value_enum(),
                    PrefixOperator::Minus => self
                        .builder
                        .build_int_neg(expr, "negtmp")?
                        .as_basic_value_enum(),
                    PrefixOperator::Not => self
                        .builder
                        .build_not(expr, "nottmp")?
                        .as_basic_value_enum(),
                })
            }
            ExprAST::LValue(lval) => {
                let (ptr, ty) = self.cgen_lvalue_load(lval)?;

                Ok(self.builder.build_load(
                    ty.into_int_type(),
                    ptr,
                    format!("load.{}", ptr.get_name().to_str().expect("")).as_str(),
                )?)
            }

            ExprAST::FunctionCall(fn_call) => {
                //
                Ok(self
                    .cgen_fn_call(fn_call)?
                    .into_int_value()
                    .as_basic_value_enum())
            }
        }
    }

    fn cgen_lvalue_load(
        &mut self,
        lval: &'ctx LValueAST,
    ) -> IRResult<(PointerValue<'ctx>, AnyTypeEnum<'ctx>)>
    {
        match lval {
            LValueAST::String(s) => {
                let str = self
                    .builder
                    .build_global_string_ptr(s, "glob.str")?
                    .as_pointer_value();

                Ok((str, str.get_type().into()))
            }

            LValueAST::Identifier(id) => {
                let name: &str = id.borrow();
                if let Some(ptr) = self.variables.get(name) {
                    Ok(*ptr)
                } else {
                    Err(IRError::String(format!("Variable {} not found", name)))
                }
            }

            LValueAST::ArraySubscript { id, expr } => {
                let expr_res = self.cgen_expresion(expr).unwrap().into_int_value();
                let (ptr, ty) = self.variables.get(*id).unwrap();

                let element_pointer = unsafe {
                    self.builder.build_gep(
                        self.int_type,
                        *ptr,
                        &[expr_res],
                        format!("gep_{}", id,).as_str(),
                    )
                };

                Ok((element_pointer?, ty.as_any_type_enum()))
            }
        }
    }

    fn cgen_fn_call(&mut self, fn_call: &'ctx FnCallAST) -> IRResult<AnyValueEnum<'ctx>>
    {
        let func = self
            .module
            .get_function(fn_call.name)
            .ok_or(IRError::String(format!(
                "Function {} not found",
                fn_call.name
            )))?;

        let func_params = func.get_params();

        if func.get_params().len() != fn_call.args.len() {
            return Err(IRError::String(format!(
                "Function {} expected {} arguments, got {}",
                fn_call.name,
                func.get_params().len(),
                fn_call.args.len()
            )));
        }

        let mut args: Vec<BasicMetadataValueEnum> = Vec::with_capacity(func_params.len());

        for (arg, param) in fn_call.args.iter().zip(func_params.iter()) {
            let arg = self.cgen_expresion(arg)?;

            // Convert int to pointer, for named arguments, lvalue_load will get us their pointer, so it can be used as reference also
            if arg.is_pointer_value() && param.is_int_value() {
                let arg_ptr = arg.into_pointer_value();

                let arg_val = self.builder.build_load(
                    self.int_type,
                    arg_ptr,
                    format!("load.{}", arg_ptr.get_name().to_str().expect("")).as_str(),
                )?;

                args.push(arg_val.into_int_value().into());
            } else {
                args.push(arg.into());
            }
        }

        Ok(self
            .builder
            .build_call(
                func,
                args.as_slice(),
                format!("call.{}", func.get_name().to_str().expect("")).as_str(),
            )?
            .as_any_value_enum())
    }

    fn cgen_statement(&mut self, stmt: &'ctx StatementAST) -> IRResult<()>
    {
        match stmt {
            StatementAST::Expr(e) => {
                self.cgen_expresion(e)?;
            }

            StatementAST::Assignment { lvalue, expr } => {
                if !matches!(lvalue, LValueAST::Identifier(_)) {
                    return Err(IRError::String(
                        "Only identifiers can be assigned to".to_string(),
                    ));
                }
                // todo: this neeeds further checking

                let expr_res = self.cgen_expresion(expr)?;
                let (lval_ptr, lval_type) = self.cgen_lvalue_load(lvalue)?;

                let e = expr_res
                    .into_int_value()
                    .const_truncate_or_bit_cast(lval_type.into_int_type());

                self.builder.build_store(lval_ptr, e)?;
            }

            StatementAST::FunctionCall(fn_call) => {
                self.cgen_fn_call(fn_call)?;
            }

            StatementAST::Return(expr) => {
                if let Some(expr) = expr {
                    let expr_res = self.cgen_expresion(expr)?;
                    self.builder.build_return(Some(&expr_res))?;
                } else {
                    self.builder.build_return(None)?;
                }
            }

            _ => todo!("cgen todo"),
        }

        Ok(())
    }
}

// ? append stdlib as bitcode ? (see here: https://github.com/hyperledger/solang/blob/06798cdeac6fd62ee98f5ae7da38f3af4933dc0f/src/emit/binary.rs#L1299)
use stdlib::LIBALAN_BITCODE as STDLIB_IR;
