use core::fmt;
use std::any::Any;
use std::borrow::{Borrow, BorrowMut};
use std::f64::consts::E;
use std::hash::Hash;
use std::path::{Path, PathBuf};

use super::ast::*;
use super::IntType as AlanIntType;

// ? append stdlib as bitcode ? (see here: https://github.com/hyperledger/solang/blob/06798cdeac6fd62ee98f5ae7da38f3af4933dc0f/src/emit/binary.rs#L1299)
use stdlib::LIBALAN_BITCODE as STDLIB_IR;

pub use inkwell::context::Context;

use inkwell::types::AnyTypeEnum;
use inkwell::values::AnyValue;
use inkwell::values::AnyValueEnum;
use inkwell::values::BasicMetadataValueEnum;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    module::Module,
    passes::PassBuilderOptions,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine},
    types::{BasicTypeEnum, IntType, PointerType, VoidType},
    values::{AsValueRef, BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace, IntPredicate,
};

use std::collections::HashMap;

use super::{IRError, IRResult, IRType};

pub struct Compiler<'ctx>
{
    // LLVM
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,

    // For Bookeeping
    variables: HashMap<&'ctx str, (PointerValue<'ctx>, IRType)>, // hash map of the addresses of variabels and their underlying types

    // Types
    int_type: IntType<'ctx>,
    char_type: IntType<'ctx>,
    bool_type: IntType<'ctx>,
    int_ptr_type: PointerType<'ctx>, // ! llvm-18 uses opaque pointers, there no point doing this fix
    char_ptr_type: PointerType<'ctx>,
    proc_type: VoidType<'ctx>,
    const_zero: IntValue<'ctx>,
}

impl<'ctx> Compiler<'ctx>
{
    pub fn llvm_version() -> String
    {
        let (major, minor, patch) = inkwell::support::get_llvm_version();
        format!("{}.{}.{}", major, minor, patch)
    }

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

        let bool_type = context.bool_type();

        let const_zero = int_type.const_zero();

        Self {
            context: context,
            builder: builder,
            module: module,

            variables: variables,

            int_type: int_type,
            char_type: char_type,
            bool_type: bool_type,
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

        self.load_stdlib();

        // Manually add the main function, to make sure ia has name of main

        let main_type = self.proc_type.fn_type(&[], false);
        let main_func = self.module.add_function("main", main_type, None);
        let basic_block = self.context.append_basic_block(main_func, "entry");

        self.builder.position_at_end(basic_block);

        for local in &program.locals {
            match local {
                LocalDefinitionAST::VarDef { name, type_ } => {
                    let ty = self.get_irtype(type_);
                    let ptr = self
                        .builder
                        .build_alloca(self.from_irtype(&ty).into_int_type(), name);

                    self.variables.insert(name, (ptr.unwrap(), ty));
                }
                LocalDefinitionAST::ArrayDef { name, type_, size } => {
                    let ty = self.get_irtype(type_);
                    let name = name.borrow();
                    let ir_type = ty.into_array_type(*size);
                    let size = self.int_type.const_int(*size as u64, false);

                    let ptr = self.builder.build_array_alloca(
                        self.from_irtype(&ty).into_int_type(),
                        size,
                        name,
                    );

                    self.variables.insert(name, (ptr.unwrap(), ir_type));
                }
                LocalDefinitionAST::FunctionDef(_) => todo!("FunctionDef"),
            }
        }
        self.builder.position_at_end(basic_block);

        // Generate function body
        for stmt in &program.body {
            self.cgen_statement(stmt)?;
        }

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
        // todo! run at least an -O0 pass, to get rid of dead functions
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

    fn get_irtype(&self, type_: &Type) -> IRType
    {
        match type_ {
            Type::Int => IRType::Int,
            Type::Byte => IRType::Byte,
            Type::Void => IRType::Void,
            Type::Array(ty) => {
                // todo: this needs checking
                //todo: also we know the size of the arrays
                self.get_irtype(ty).into_array_type(0)
            }
            Type::Ref(ty) => self.get_irtype(ty).into_reference_type(),
        }
    }

    fn from_irtype(&self, ty: &IRType) -> AnyTypeEnum<'ctx>
    {
        match ty {
            IRType::Int => self.int_type.into(),
            IRType::Byte => self.char_type.into(),
            IRType::Void => self.proc_type.into(),
            IRType::Array(ty, size) => {
                let inner = self.from_irtype(ty);
                match inner {
                    AnyTypeEnum::IntType(t) => t.array_type(*size as u32).into(),
                    AnyTypeEnum::PointerType(t) => t.array_type(*size as u32).into(),
                    _ => todo!(),
                }
            }
            IRType::Pointer(ty) => self.from_irtype(ty),

            _ => todo!(),
        }
    }

    fn int_type_to_irtype(&self, ty: IntValue<'ctx>) -> IRType
    {
        match ty.get_type().get_bit_width() {
            8 => IRType::Byte,
            _ => IRType::Int, // good enough for Australia (for now)
        }
    }

    fn any_value_enum_to_irtype(&self, ty: AnyValueEnum<'ctx>) -> IRType
    {
        match ty {
            AnyValueEnum::IntValue(_) => self.int_type_to_irtype(ty.into_int_value()),
            AnyValueEnum::PointerValue(_) => {
                IRType::Pointer(Box::new(self.any_value_enum_to_irtype(ty)))
            }
            _ => unimplemented!(),
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

    fn cgen_expresion(
        &mut self,
        expr: &'ctx ExprAST,
    ) -> IRResult<(BasicValueEnum<'ctx>, Option<IRType>)>
    {
        match expr {
            ExprAST::Error => Err(IRError::UnknownError),
            ExprAST::Literal(lit) => Ok((self.cgen_literal(lit)?.as_basic_value_enum(), None)),
            ExprAST::InfixOp { lhs, op, rhs } => {
                let lhs = self.cgen_expresion(lhs)?;
                let rhs = self.cgen_expresion(rhs)?;

                if !lhs.0.is_int_value() {
                    return Err(IRError::String(
                        "Infix operator requires both operands to be integers".to_string(),
                    ));
                }
                if !rhs.0.is_int_value() {
                    return Err(IRError::String(
                        "Infix operator requires both operands to be integers".to_string(),
                    ));
                }
                let lhs = lhs.0.into_int_value();
                let rhs = rhs.0.into_int_value();

                if lhs.get_type() != rhs.get_type() {
                    return Err(IRError::String(
                        format!(
                            "Infix operator {:?} requires both operands to have the same type, got {} and {}",
                            op, lhs.get_type(), rhs.get_type()
                        ).to_string()
                    ));
                }

                Ok((
                    match op {
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
                    .as_basic_value_enum(),
                    None,
                ))
            }
            ExprAST::PrefixOp { op, expr } => {
                let expr = self.cgen_expresion(expr)?;
                if !expr.0.is_int_value() {
                    return Err(IRError::String(
                        "Prefix operator requires the operand to be an integer".to_string(),
                    ));
                }
                let expr = expr.0.into_int_value();

                Ok((
                    match op {
                        PrefixOperator::Plus => expr.as_basic_value_enum(),
                        PrefixOperator::Minus => self
                            .builder
                            .build_int_neg(expr, "negtmp")?
                            .as_basic_value_enum(),
                        PrefixOperator::Not => self
                            .builder
                            .build_not(expr, "nottmp")?
                            .as_basic_value_enum(),
                    },
                    None,
                ))
            }
            ExprAST::LValue(lval) => {
                let (ptr, ty) = self.cgen_lvalue_load(lval)?;

                if matches!(lval, LValueAST::String { .. }) {
                    Ok((ptr.as_basic_value_enum(), Some(ty)))
                } else {
                    Ok((
                        self.builder.build_load(
                            self.from_irtype(&ty).into_int_type(),
                            ptr,
                            format!("load.{}", ptr.get_name().to_str().expect("")).as_str(),
                        )?,
                        Some(ty),
                    ))
                }
            }

            ExprAST::FunctionCall(fn_call) => Ok((
                self.cgen_fn_call(fn_call)?
                    .into_int_value()
                    .as_basic_value_enum(),
                None,
            )),
        }
    }

    fn cgen_lvalue_load(&mut self, lval: &'ctx LValueAST)
        -> IRResult<(PointerValue<'ctx>, IRType)>
    {
        match lval {
            LValueAST::String(s) => {
                let str = self
                    .builder
                    .build_global_string_ptr(s, "glob.str")?
                    .as_pointer_value();

                Ok((
                    str,
                    IRType::Array(Box::new(IRType::Byte), (s.len() + 1) as i32), // !!!! this isn't correct (maybe string type makes sense ?)
                ))
            }

            LValueAST::Identifier(id) => {
                let name: &str = id.borrow();
                if let Some(ptr) = self.variables.get(name) {
                    Ok(ptr.clone())
                } else {
                    Err(IRError::String(format!("undeclared undeclared '{}'", name)))
                }
            }

            LValueAST::ArraySubscript { id, expr } => {
                let expr_res = self.cgen_expresion(expr)?;

                if !expr_res.0.is_int_value() {
                    return Err(IRError::String(
                        "Array subscript requires an integer index".to_string(),
                    ));
                }
                let expr_res = expr_res.0.into_int_value();

                let (ptr, ty) = self
                    .variables
                    .get(*id)
                    .ok_or(IRError::String(format!("undeclared undeclared '{}'", id)))?;

                // make sure we are subscripting an array
                if !ty.is_array() {
                    return Err(IRError::String(format!(
                        "identifier '{}' is not an array, (expected array found {})",
                        id, ty
                    )));
                }
                let iner_type = ty.get_inner_type().unwrap();

                let element_pointer = unsafe {
                    self.builder.build_in_bounds_gep(
                        self.from_irtype(ty).into_array_type(), // ? this should be ok for now, we only have 1D arrays
                        *ptr,
                        &[self.const_zero, expr_res],
                        format!("arraysub.{}", id).as_str(),
                    )
                };

                Ok((element_pointer?, iner_type.clone()))
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
            let (arg, arg_ty) = self.cgen_expresion(arg)?;

            if arg.get_type() != param.get_type() {
                return Err(IRError::String(format!(
                    "Cannot pass argument of type '{}' to function '{}', expected '{}'",
                    self.any_value_enum_to_irtype(arg.into()),
                    fn_call.name,
                    self.any_value_enum_to_irtype(param.as_any_value_enum())
                )));
            }

            if arg.is_pointer_value() && param.is_int_value() {
                // Convert int to pointer, for named arguments, lvalue_load will get us their pointer, so it can be used as reference also

                if let Some(ty) = arg_ty {
                    if ty.is_array() {
                        return Err(IRError::String(format!(
                            "Cannot pass pointer of '{}' as argument to function '{}', expected a '{}'",
                            ty,
                            fn_call.name,
                            self.int_type_to_irtype(param.into_int_value())
                        )));
                    }
                }

                let arg_ptr = arg.into_pointer_value();

                let arg_val = self.builder.build_load(
                    param.get_type(),
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

    fn cgen_condition(&mut self, cond: &'ctx ConditionAST) -> IRResult<IntValue<'ctx>>
    {
        match cond {
            ConditionAST::BoolConst(b) => Ok(self.bool_type.const_int(*b as u64, false)),

            ConditionAST::InfixLogicOp { lhs, op, rhs } => {
                let lhs = self.cgen_condition(lhs)?;
                let rhs = self.cgen_condition(rhs)?;

                Ok(match op {
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
                let lhs = self.cgen_expresion(lhs)?;
                let rhs = self.cgen_expresion(rhs)?;

                if !lhs.0.is_int_value() {
                    // todo: Could improve this error message
                    return Err(IRError::String(format!(
                        "Infix operator requires both operands to be integers"
                    )));
                }
                if !rhs.0.is_int_value() {
                    return Err(IRError::String(format!(
                        "Infix operator requires both operands to be integers"
                    )));
                }
                let lhs = lhs.0.into_int_value();
                let rhs = rhs.0.into_int_value();

                if lhs.get_type() != rhs.get_type() {
                    return Err(IRError::String(
                        format!(
                            "Infix operator {:?} requires both operands to have the same type, got {} and {}",
                            op, lhs.get_type(), rhs.get_type()
                        ).to_string()
                    ));
                }

                Ok(match op {
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
                    _ => panic!("Should not happen"),
                }?)
            }

            ConditionAST::Error => Err(IRError::UnknownError),
        }
    }

    fn cgen_statement(&mut self, stmt: &'ctx StatementAST) -> IRResult<()>
    {
        match stmt {
            StatementAST::Expr(e) => {
                self.cgen_expresion(e)?;
            }

            StatementAST::Assignment { lvalue, expr } => {
                if matches!(lvalue, LValueAST::String(_)) {
                    return Err(IRError::String(
                        "Cannot assign to a string constant".to_string(),
                    ));
                }
                // todo: this neeeds further checking
                let (lval_ptr, lval_type) = self.cgen_lvalue_load(lvalue)?;

                if !(lval_type.is_int() || lval_type.is_byte()) {
                    return Err(IRError::String(format!(
                        "Cannot assign to a non-integral type {}",
                        lval_type
                    )));
                }

                let expr_res = self.cgen_expresion(expr)?;

                let e = expr_res
                    .0
                    .into_int_value()
                    .const_truncate_or_bit_cast(self.from_irtype(&lval_type).into_int_type());

                self.builder.build_store(lval_ptr, e)?;
            }

            StatementAST::FunctionCall(fn_call) => {
                self.cgen_fn_call(fn_call)?;
            }

            StatementAST::Return(expr) => {
                if let Some(expr) = expr {
                    let expr_res = self.cgen_expresion(expr)?;
                    self.builder.build_return(Some(&expr_res.0))?;
                } else {
                    self.builder.build_return(None)?;
                }
            }

            StatementAST::If {
                condition,
                then,
                else_,
            } => {
                let block = self.builder.get_insert_block().unwrap();
                let current_function = block.get_parent().unwrap();

                let cond = self.cgen_condition(condition)?;

                let if_block = self.context.append_basic_block(current_function, "if.then");
                let end_block: BasicBlock =
                    self.context.append_basic_block(current_function, "if.end");

                if let Some(_) = else_ {
                    let else_block = self.context.append_basic_block(current_function, "if.else");
                    self.builder
                        .build_conditional_branch(cond, if_block, else_block)?;
                    // Build then block
                    self.builder.position_at_end(if_block);
                    self.cgen_statement(then)?;
                    self.builder.build_unconditional_branch(end_block)?;

                    // Build else block
                    self.builder.position_at_end(else_block);
                    self.cgen_statement(else_.as_ref().unwrap())?;
                    self.builder.build_unconditional_branch(end_block)?;
                } else {
                    self.builder
                        .build_conditional_branch(cond, if_block, end_block)?;

                    // Build then block
                    self.builder.position_at_end(if_block);
                    self.cgen_statement(then)?;
                    self.builder.build_unconditional_branch(end_block)?;
                }

                // End block
                self.builder.position_at_end(end_block);
            }

            StatementAST::While { condition, body } => {
                let block = self.builder.get_insert_block().unwrap();
                let current_function = block.get_parent().unwrap();

                let while_cond = self
                    .context
                    .append_basic_block(current_function, "while.cond");
                let while_body = self
                    .context
                    .append_basic_block(current_function, "while.body");
                let while_end = self
                    .context
                    .append_basic_block(current_function, "while.end");

                // Build condition block

                self.builder.build_unconditional_branch(while_cond)?;
                self.builder.position_at_end(while_cond);

                let cond = self.cgen_condition(condition)?;
                self.builder
                    .build_conditional_branch(cond, while_body, while_end)?;

                // Build body block
                self.builder.position_at_end(while_body);
                self.cgen_statement(body)?;

                self.builder.build_unconditional_branch(while_cond)?;

                // Build end block
                self.builder.position_at_end(while_end);
            }

            StatementAST::Compound(stmts) => {
                for stmt in stmts {
                    self.cgen_statement(stmt)?;
                }
            }
            StatementAST::Error => return Err(IRError::UnknownError),
            StatementAST::Null => {}
        }

        Ok(())
    }
}
