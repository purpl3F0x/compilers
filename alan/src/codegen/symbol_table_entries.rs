use super::irtype::IRType;
use inkwell::values::{FunctionValue, PointerValue};
use std::collections::HashMap;

// todo: Implement Span

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LValueEntry<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub ty: IRType,
    // pub span: Span,
}

impl<'ctx> LValueEntry<'ctx> {
    pub fn new(ptr: PointerValue<'ctx>, ty: IRType) -> LValueEntry {
        LValueEntry { ptr, ty }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionEntry<'ctx> {
    pub function: FunctionValue<'ctx>,
    pub return_ty: IRType,
    pub param_tys: Vec<IRType>,
    pub captures: HashMap<&'ctx str, IRType>,
    // pub span: Span,
}

impl<'ctx> FunctionEntry<'ctx> {
    pub fn new(
        function: FunctionValue<'ctx>,
        return_ty: IRType,
        param_tys: Vec<IRType>,
        captures: HashMap<&'ctx str, IRType>,
    ) -> FunctionEntry<'ctx> {
        FunctionEntry { function, return_ty, param_tys, captures }
    }

    pub fn new_extern(function: FunctionValue<'ctx>, return_ty: IRType, param_tys: Vec<IRType>) -> FunctionEntry<'ctx> {
        FunctionEntry { function, return_ty, param_tys, captures: HashMap::new() }
    }

    pub fn total_num_params(&self) -> usize {
        self.param_tys.len() + self.captures.len()
    }
}
