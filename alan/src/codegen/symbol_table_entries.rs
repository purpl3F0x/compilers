use super::irtype::IRType;
use super::Span;
use std::collections::HashMap;

use inkwell::values::{FunctionValue, PointerValue};

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LValueEntry<'ctx> {
    pub ptr: PointerValue<'ctx>,
    pub ty: IRType,
    /// The span of the declaration of the lvalue
    pub span: Span,
}

impl<'ctx> LValueEntry<'ctx> {
    pub fn new(ptr: PointerValue<'ctx>, ty: IRType, span: Span) -> LValueEntry {
        LValueEntry { ptr, ty, span }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionEntry<'ctx> {
    pub function: FunctionValue<'ctx>,
    pub return_ty: IRType,
    pub param_tys: Vec<IRType>,
    pub captures: Option<HashMap<&'ctx str, IRType>>,
    pub is_stdlib: bool,
    /// The span of the declaration of the function
    /// (stdlib/extern) functions have a span of 0,  0) - to avoid wasting memory for Option<Span>
    pub span: Span,
    pub ret_ty_span: Span,
}

impl<'ctx> FunctionEntry<'ctx> {
    pub fn new(
        function: FunctionValue<'ctx>,
        return_ty: IRType,
        param_tys: Vec<IRType>,
        captures: HashMap<&'ctx str, IRType>,
        span: Span,
        ret_ty_span: Span,
    ) -> FunctionEntry<'ctx> {
        FunctionEntry { function, return_ty, param_tys, captures: Some(captures), is_stdlib: false, span, ret_ty_span }
    }

    pub fn new_extern(function: FunctionValue<'ctx>, return_ty: IRType, param_tys: Vec<IRType>) -> FunctionEntry<'ctx> {
        FunctionEntry {
            function,
            return_ty,
            param_tys,
            captures: None,
            is_stdlib: true,
            span: Span::new(0, 0),
            ret_ty_span: Span::new(0, 0),
        }
    }

    pub fn total_num_params(&self) -> usize {
        if let Some(captures) = &self.captures {
            self.param_tys.len() + captures.len()
        } else {
            self.param_tys.len()
        }
    }

    pub fn signature_string(&self) -> String {
        let mut s = String::new();
        s.push_str("(");
        for (i, param_ty) in self.param_tys.iter().enumerate() {
            if i != 0 {
                s.push_str(", ");
            }
            s.push_str(&param_ty.get_name());
        }
        s.push_str("):");
        s.push_str(&self.return_ty.get_name());

        s
    }
}

impl<'ctx> std::fmt::Display for FunctionEntry<'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "(")?;

        for (i, param) in self.param_tys.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", param)?;
        }
        write!(f, "): {}", self.return_ty)
    }
}
