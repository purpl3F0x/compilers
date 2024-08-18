#[derive(Debug, Clone, Hash, Eq)]
pub enum IRType {
    Void,
    Byte,
    Int,
    // String,
    Array(Box<Self>, super::IntType),
    Reference(Box<Self>),
    Pointer(Box<Self>),
}

impl IRType {
    pub fn get_name(&self) -> String {
        match self {
            Self::Void => "proc".to_string(),
            Self::Byte => "byte".to_string(),
            Self::Int => "int".to_string(),
            Self::Array(t, size) => {
                if *size > 0 {
                    format!("{}[{}]", t.get_name(), size)
                } else {
                    format!("{}[]", t.get_name())
                }
            }
            Self::Reference(t) => format!("{} reference", t.get_name()),
            Self::Pointer(t) => format!("{}[]", t.get_name()),
        }
    }

    #[inline]
    pub fn is_byte(&self) -> bool {
        matches!(self, Self::Byte)
    }

    #[inline]
    pub fn is_int(&self) -> bool {
        matches!(self, Self::Int)
    }

    #[inline]
    pub fn is_void(&self) -> bool {
        matches!(self, Self::Void)
    }

    #[inline]
    pub fn is_array(&self) -> bool {
        matches!(self, Self::Array(_, _))
    }

    #[inline]
    pub fn is_reference(&self) -> bool {
        matches!(self, Self::Reference(_))
    }

    #[inline]
    pub fn is_primitive_reference(&self) -> bool {
        matches!(self, Self::Reference(t) if t.is_byte() || t.is_int())
    }

    #[inline]
    pub fn is_primitive(&self) -> bool {
        self.is_byte() || self.is_int()
    }

    #[inline]
    pub fn is_pointer(&self) -> bool {
        matches!(self, Self::Pointer(_))
    }

    #[inline]
    pub fn into_array_type(&self, size: i32) -> Self {
        Self::Array(Box::new(self.clone()), size)
    }

    #[inline]
    pub fn into_reference_type(&self) -> Self {
        Self::Reference(Box::new(self.clone()))
    }

    #[inline]
    pub fn into_pointer_type(&self) -> Self {
        Self::Pointer(Box::new(self.clone()))
    }

    #[inline]
    pub fn get_inner_type(&self) -> Option<&Self> {
        match self {
            Self::Array(t, _) | Self::Reference(t) | Self::Pointer(t) => Some(t),
            _ => None,
        }
    }
}

impl PartialEq for IRType {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Compare the Array variant by element type only, ignoring size
            (IRType::Array(self_type, _), IRType::Array(other_type, _)) => self_type == other_type,
            // Arrays are pointer
            (IRType::Pointer(self_ptr), IRType::Array(other_ptr, _)) => self_ptr == other_ptr,
            (IRType::Array(self_ptr, _), IRType::Pointer(other_ptr)) => self_ptr == other_ptr,
            // For other variants, use regular equality
            (IRType::Void, IRType::Void) => true,
            (IRType::Byte, IRType::Byte) => true,
            (IRType::Int, IRType::Int) => true,
            (IRType::Reference(self_ref), IRType::Reference(other_ref)) => self_ref == other_ref,
            (IRType::Pointer(self_ptr), IRType::Pointer(other_ptr)) => self_ptr == other_ptr,
            // If the variants don't match, return false
            _ => false,
        }
    }
}

impl std::fmt::Display for IRType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.get_name())
    }
}

//
// tests //
//
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eq_for_primitive_types() {
        assert_eq!(IRType::Void, IRType::Void);
        assert_eq!(IRType::Byte, IRType::Byte);
        assert_eq!(IRType::Int, IRType::Int);
        assert_ne!(IRType::Void, IRType::Byte);
        assert_ne!(IRType::Int, IRType::Byte);
    }

    #[test]
    fn test_eq_for_array_type() {
        let array1 = IRType::Array(Box::new(IRType::Byte), 10);
        let array2 = IRType::Array(Box::new(IRType::Byte), 20);
        let array3 = IRType::Array(Box::new(IRType::Int), 10);

        assert_eq!(array1, array2); // Element type matches, size is ignored
        assert_ne!(array1, array3); // Element type differs
    }

    #[test]
    fn test_eq_for_reference_type() {
        let ref1 = IRType::Reference(Box::new(IRType::Int));
        let ref2 = IRType::Reference(Box::new(IRType::Int));
        let ref3 = IRType::Reference(Box::new(IRType::Byte));

        assert_eq!(ref1, ref2); // Element type matches
        assert_ne!(ref1, ref3); // Element type differs
    }

    #[test]
    fn test_eq_for_pointer_type() {
        let ptr1 = IRType::Pointer(Box::new(IRType::Byte));
        let ptr2 = IRType::Pointer(Box::new(IRType::Byte));
        let ptr3 = IRType::Pointer(Box::new(IRType::Int));

        assert_eq!(ptr1, ptr2); // Element type matches
        assert_ne!(ptr1, ptr3); // Element type differs
    }

    #[test]
    fn test_eq_pointer_and_array() {
        let ptr = IRType::Pointer(Box::new(IRType::Byte));
        let array = IRType::Array(Box::new(IRType::Byte), 10);

        assert_eq!(ptr, array); // Pointer should equal Array when types match
        assert_eq!(array, ptr); // Pointer should equal Array when types match
    }

    #[test]
    fn test_get_name() {
        assert_eq!(IRType::Void.get_name(), "proc");
        assert_eq!(IRType::Byte.get_name(), "byte");
        assert_eq!(IRType::Int.get_name(), "int");
        assert_eq!(IRType::Array(Box::new(IRType::Int), 10).get_name(), "int[10]");
        assert_eq!(IRType::Array(Box::new(IRType::Int), 0).get_name(), "int[]");
        assert_eq!(IRType::Reference(Box::new(IRType::Byte)).get_name(), "byte reference");
        assert_eq!(IRType::Pointer(Box::new(IRType::Int)).get_name(), "int[]");
    }

    #[test]
    fn test_is_methods() {
        let byte = IRType::Byte;
        let int = IRType::Int;
        let void = IRType::Void;
        let array = IRType::Array(Box::new(IRType::Byte), 10);
        let reference = IRType::Reference(Box::new(IRType::Int));
        let pointer = IRType::Pointer(Box::new(IRType::Byte));

        assert!(byte.is_byte());
        assert!(int.is_int());
        assert!(void.is_void());
        assert!(array.is_array());
        assert!(reference.is_reference());
        assert!(pointer.is_pointer());

        assert!(reference.is_primitive_reference());
        assert!(byte.is_primitive());
        assert!(int.is_primitive());
    }

    #[test]
    fn test_into_type_conversions() {
        let int = IRType::Int;
        let array = int.into_array_type(10);
        let reference = array.into_reference_type();
        let pointer = reference.into_pointer_type();

        assert!(matches!(array, IRType::Array(_, 10)));
        assert!(matches!(reference, IRType::Reference(_)));
        assert!(matches!(pointer, IRType::Pointer(_)));
    }

    #[test]
    fn test_get_inner_type() {
        let byte = IRType::Byte;
        let array = IRType::Array(Box::new(byte.clone()), 10);
        let reference = IRType::Reference(Box::new(byte.clone()));
        let pointer = IRType::Pointer(Box::new(byte.clone()));

        assert_eq!(array.get_inner_type(), Some(&byte));
        assert_eq!(reference.get_inner_type(), Some(&byte));
        assert_eq!(pointer.get_inner_type(), Some(&byte));
        assert_eq!(byte.get_inner_type(), None);
    }
}
