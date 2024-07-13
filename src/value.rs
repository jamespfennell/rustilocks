use loxstring::LoxString;
use std::fmt::Display;

#[derive(PartialEq, Clone, Copy, Debug)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
    String(LoxString),
}

impl Value {
    pub fn is_falsey(&self) -> bool {
        match self {
            Value::Nil => true,
            Value::Bool(b) => !b,
            _ => false,
        }
    }

    pub fn equal(self, other: Value) -> bool {
        match (self, other) {
            (Value::Number(lhs), Value::Number(rhs)) => lhs == rhs,
            (Value::Bool(lhs), Value::Bool(rhs)) => lhs == rhs,
            (Value::Nil, Value::Nil) => true,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Number(d) => d.fmt(f),
            Value::Bool(b) => b.fmt(f),
            Value::Nil => "nil".fmt(f),
            Value::String(s) => s.fmt(f),
        }
    }
}

/// The string module implements Lox strings.
pub mod loxstring {
    use std::collections::HashSet;
    use std::fmt::Display;
    use std::hash::Hash;

    #[derive(Debug, Clone, Copy)]
    pub struct LoxString(&'static str);

    impl LoxString {
        /// Returns the Lox string as string slice.
        ///
        /// The full lifetime of the slice is until the Lox string is garbage collected.
        /// Garbage collection requires a mutable reference to the underlying interner.
        /// Calling this function forces the caller to pass a reference to the interner, which
        /// then blocks garbage collection while the return value is being used.
        /// Essentially, we're trying to use the Rust borrow checker to introduce some safetey into the GC.
        /// This is not foolproof as the method current accepts _any_ interner.
        pub fn as_str<'a>(&self, _: &'a Interner) -> &'a str {
            self.0
        }

        pub fn len(&self) -> usize {
            self.0.len()
        }
    }

    impl PartialEq for LoxString {
        fn eq(&self, other: &Self) -> bool {
            // Because strings are interned, we can compare them using a fast pointer comparison.
            std::ptr::eq(self.0, other.0)
        }
    }

    impl Eq for LoxString {}

    impl Hash for LoxString {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            // Because strings are interned, we can compare them using a fast pointer comparison.
            std::ptr::hash(self.0, state)
        }
    }

    impl Display for LoxString {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            self.0.fmt(f)
        }
    }

    #[derive(Default, Debug)]
    pub struct Interner {
        strings: HashSet<&'static str>,
    }

    impl Interner {
        pub fn intern_ref(&mut self, s: &str) -> LoxString {
            if let Some(s) = self.strings.get(s) {
                return LoxString(s);
            }
            self.intern_owned(s.into())
        }

        pub fn intern_owned(&mut self, s: String) -> LoxString {
            if let Some(s) = self.strings.get(&s as &str) {
                return LoxString(s);
            }
            let b = Box::leak(s.into_boxed_str());
            self.strings.insert(b);
            LoxString(b)
        }
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_equality() {
            let s_1: String = "value".into();
            let s_2: String = "value".into();

            let mut interner: Interner = Default::default();
            let l_1 = interner.intern_owned(s_1);
            let l_2 = interner.intern_owned(s_2);

            assert_eq!(l_1, l_2);
        }
    }
}
