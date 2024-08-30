use super::{IRType, Span};

#[derive(Debug, PartialEq)]
pub enum SemanticError {
    TopIsNotAProc {
        signature_span: Span,
        ret_ty: IRType,
        r_ty_span: Span,
    },
    TopHasArguments {
        signature_span: Span,
    },
    AssignResultOfProc {
        span: Span,
        decl_span: Span,
    },
    UndeclaredIdentifier {
        span: Span,
        name: String,
    },
    SubscriptingNonArray {
        span: Span,
        ty: IRType,
        decl_span: Span,
    },
    MismatchedArgumentCount {
        span: Span,
        expected: usize,
        found: usize,
        decl_span: Span,
        func_signature: String,
    },
    UndeclaredFunction {
        span: Span,
        name: String,
    },
    ArgumentTypeMismatch {
        span: Span,
        arg_index: usize,
        expected: IRType,
        found: IRType,
        decl_span: Span,
        func_signature: String,
    },
    ConstantAsRef {
        span: Span,
        ty: IRType,
        ref_ty: IRType,
        decl_span: Span,
    },
    AssignToString {
        span: Span,
    },
    AssignTypeMismatch {
        span: Span,
        expected: IRType,
        found: IRType,
        decl_span: Span,
    },
    UnusedReturnValue {
        span: Span,
        ty: IRType,
        decl_span: Span,
        ret_ty_span: Span,
    },
    MismatchedReturnType {
        span: Span,
        expected: IRType,
        found: IRType,
    },
    RedeclaringIdentifier {
        span: Span,
        name: String,
        decl_span: Span,
    },
    RedeclaringFunction {
        span: Span,
        name: String,
        decl_span: Span,
    },
    ReachEndOfNonVoidFunction {
        span: Span,
        ret_ty: IRType,
    },
    MultipleArgumentsWithSameName {
        span: Span,
        name: String,
        span1: Span,
        span2: Span,
    },
    PtrAsPrimitive {
        span: Span,
        ty: IRType,
    },

    MismatchExprTypes {
        span: Span,
        lhs: IRType,
        rhs: IRType,
        lhs_span: Span,
        rhs_span: Span,
        lhs_decl_span: Option<Span>,
        rhs_decl_span: Option<Span>,
    },

    ArrayAsNonRef {
        span: Span,
        ty: IRType,
        decl_span: Span,
    },
}

impl std::fmt::Display for SemanticError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        // Will just use this as a boilerplate, since we will not be using fmt for SemanticErrors
        write!(f, "{:?}", self)
    }
}
