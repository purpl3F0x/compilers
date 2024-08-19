pub use inkwell::builder::BuilderError;
pub use inkwell::support::LLVMString;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum IRError {
    UnknownError,
    BuilderError(BuilderError),
    String(String),
}

pub type IRResult<T> = Result<T, IRError>;

impl From<BuilderError> for IRError {
    fn from(error: BuilderError) -> Self {
        IRError::BuilderError(error)
    }
}

impl From<LLVMString> for IRError {
    fn from(error: LLVMString) -> Self {
        IRError::String(error.to_string())
    }
}

impl From<String> for IRError {
    fn from(error: String) -> Self {
        IRError::String(error)
    }
}

impl std::fmt::Display for IRError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            IRError::UnknownError => write!(f, "Unknown error"),
            IRError::BuilderError(e) => write!(f, "Builder error: {}", e),
            IRError::String(s) => write!(f, "{}", s),
        }
    }
}
