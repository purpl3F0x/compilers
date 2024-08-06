use core::{error, fmt};
use std::{borrow::Borrow, fmt::Error};

use super::ast::*;
use super::IntType as AlanIntType;

use inkwell::builder::BuilderError;
pub use inkwell::context::Context;

use inkwell::support::LLVMString;
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
    passes::{PassManager, PassManagerBuilder},
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{BasicType, BasicTypeEnum, IntType, PointerType, VoidType},
    values::{AsValueRef, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace, FloatPredicate, IntPredicate, OptimizationLevel,
    OptimizationLevel::Aggressive,
};

use llvm::LLVMValue;
use llvm_sys as llvm;
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

    variables: HashMap<String, PointerValue<'ctx>>,
    // environment: Vec<HashMap<String, BasicValueEnum<'ctx>>>,
    int_type: IntType<'ctx>,
    char_type: IntType<'ctx>,
    int_ptr_type: PointerType<'ctx>,
    char_ptr_type: PointerType<'ctx>,
    proc_type: VoidType<'ctx>,
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
        }
    }

    pub fn optimize(&self)
    {
        let config = InitializationConfig::default();

        Target::initialize_all(&config);

        // Create a pass manager
        // let pass_manager_builder = PassManagerBuilder::create();
        // let pass_manager: PassManager<Module> = PassManager::create(());

        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();

        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                OptimizationLevel::Aggressive,
                RelocMode::PIC,
                CodeModel::Default,
            )
            .unwrap();

        const PASSES: &str = "instcombine,reassociate,gvn,simplifycfg,\
                            loop-simplify,loop-rotate,loop-unroll,loop-unroll-and-jam,loop-deletion,loop-reduce,simple-loop-unswitch,loop-vectorize,\
                            mem2reg,dce,licm,adce,memcpyopt,gvn";

        self.module
            .run_passes(PASSES, &target_machine, PassBuilderOptions::create())
            .unwrap();
    }

    pub fn compile(&mut self, program: &'ctx FunctionAST) -> IRResult<()>
    {
        // TODO: add the stdlib functions
        self.append_std_functions();

        // Manually add the main function, to make sure ia has name of main

        let main_type = self.proc_type.fn_type(&[], false);
        let main_func = self.module.add_function("main", main_type, None);
        let basic_block = self.context.append_basic_block(main_func, "entry");

        self.builder.position_at_end(basic_block);

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
        let target_triple = TargetMachine::get_default_triple();
        Target::initialize_all(&InitializationConfig::default());

        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target
            .create_target_machine(
                &target_triple,
                "generic",
                "",
                inkwell::OptimizationLevel::None,
                inkwell::targets::RelocMode::Default,
                inkwell::targets::CodeModel::Default,
            )
            .expect("Could not create target machine");

        let buf: inkwell::memory_buffer::MemoryBuffer = target_machine
            .write_to_memory_buffer(&self.module, inkwell::targets::FileType::Assembly)
            .unwrap();

        String::from_utf8(buf.as_slice().to_vec()).unwrap()
    }

    fn append_std_functions(&self)
    {
        let write_interger_t = self.proc_type.fn_type(&[self.int_type.into()], false);
        self.module
            .add_function("writeInteger", write_interger_t, None);
        let write_byte_t = self.proc_type.fn_type(&[self.char_type.into()], false);
        self.module.add_function("writeByte", write_byte_t, None);

        let write_char_t = self.proc_type.fn_type(&[self.char_type.into()], false);
        self.module.add_function("writeChar", write_char_t, None);

        let write_string_t = self.proc_type.fn_type(&[self.char_ptr_type.into()], false);
        self.module
            .add_function("writeString", write_string_t, None);
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
            ExprAST::LValue(lval) => Ok(self.cgen_lvalue_load(lval)?.as_basic_value_enum()),

            ExprAST::FunctionCall(fn_call) => {
                //
                Ok(self
                    .cgen_fn_call(fn_call)?
                    .into_int_value()
                    .as_basic_value_enum())
            }
        }
    }

    fn cgen_lvalue_load(&mut self, lval: &'ctx LValueAST) -> IRResult<PointerValue<'ctx>>
    {
        match lval {
            LValueAST::String(s) => Ok(self
                .builder
                .build_global_string_ptr(s, "tmpstr")?
                .as_pointer_value()),

            LValueAST::Identifier(id) => {
                let name = id.borrow();
                let ptr: &PointerValue<'ctx> = self.variables.get(name).unwrap();
                Ok(self
                    .builder
                    .build_load(self.int_ptr_type, *ptr, name)? // ! Fix Type (??)
                    .into_pointer_value())
            }
            LValueAST::ArraySubscript { id, expr } => {
                let expr_res = self.cgen_expresion(expr)?;
                let ptr: &PointerValue<'ctx> = self.variables.get(*id).unwrap();
                let element_pointer = unsafe {
                    self.builder.build_gep(
                        self.int_ptr_type,
                        *ptr,
                        &[expr_res.into_int_value()],
                        "element_ptr",
                    )
                };

                Ok(element_pointer?.into())
            }
        }
    }

    fn cgen_fn_call(&mut self, fn_call: &'ctx FnCallAST) -> IRResult<AnyValueEnum<'ctx>>
    {
        let func = self.module.get_function(fn_call.name).unwrap();
        let args: Vec<BasicMetadataValueEnum> = fn_call
            .args
            .iter()
            .map(|arg| self.cgen_expresion(arg).unwrap().into())
            .collect();

        Ok(self
            .builder
            .build_call(func, args.as_slice(), "calltmp")?
            .as_any_value_enum())
    }

    fn cgen_statement(&mut self, stmt: &'ctx StatementAST) -> IRResult<()>
    {
        println!("Statement: {:?}", stmt);
        match stmt {
            StatementAST::Expr(e) => {
                self.cgen_expresion(e)?;
            }

            StatementAST::Assignment { lvalue, expr } => {
                let lval_ptr = self.cgen_lvalue_load(lvalue)?;
                let expr_res = self.cgen_expresion(expr)?;

                self.builder.build_store(lval_ptr, expr_res)?;
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
