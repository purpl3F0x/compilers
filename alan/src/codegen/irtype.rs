#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub enum IRType
{
    Void,
    Byte,
    Int,
    // String,
    Array(Box<Self>, super::IntType),
    Reference(Box<Self>),
    Pointer(Box<Self>),
}

impl IRType
{
    pub fn get_name(&self) -> String
    {
        match self {
            Self::Void => "void".to_string(),
            Self::Byte => "byte".to_string(),
            Self::Int => "int".to_string(),
            Self::Array(t, size) => format!("{}[{}]", t.get_name(), size),
            Self::Reference(t) => format!("ref {}", t.get_name()),
            Self::Pointer(t) => format!("ptr {}", t.get_name()),
        }
    }

    #[inline]
    pub fn is_byte(&self) -> bool
    {
        matches!(self, Self::Byte)
    }

    #[inline]
    pub fn is_int(&self) -> bool
    {
        matches!(self, Self::Int)
    }

    #[inline]
    pub fn is_void(&self) -> bool
    {
        matches!(self, Self::Void)
    }

    #[inline]
    pub fn is_array(&self) -> bool
    {
        matches!(self, Self::Array(_, _))
    }

    #[inline]
    pub fn is_reference(&self) -> bool
    {
        matches!(self, Self::Reference(_))
    }

    #[inline]
    pub fn is_pointer(&self) -> bool
    {
        matches!(self, Self::Pointer(_))
    }

    #[inline]
    pub fn into_array_type(&self, size: i32) -> Self
    {
        Self::Array(Box::new(self.clone()), size)
    }

    #[inline]
    pub fn into_reference_type(&self) -> Self
    {
        Self::Reference(Box::new(self.clone()))
    }

    #[inline]
    pub fn into_pointer_type(&self) -> Self
    {
        Self::Pointer(Box::new(self.clone()))
    }

    #[inline]
    pub fn get_inner_type(&self) -> Option<&Self>
    {
        match self {
            Self::Array(t, _) | Self::Reference(t) | Self::Pointer(t) => Some(t),
            _ => None,
        }
    }
}

impl std::fmt::Display for IRType
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result
    {
        write!(f, "{}", self.get_name())
    }
}
