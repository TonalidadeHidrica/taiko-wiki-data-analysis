#[macro_export]
macro_rules! pcre {
    ($pattern: expr $(, $($flags: ident)*)?) => {{
        use std::cell::RefCell;
        use pcre::Pcre;
        thread_local! {
            static PATTERN: RefCell<Pcre> = {
                let flags = $crate::flags!($($($flags)*)?);
                RefCell::new(Pcre::compile_with_options($pattern, &flags).unwrap())
            };
        }
        PATTERN
    }};

    ($pattern: expr $(, $($flags: ident)*)? => $method: ident($($args: expr),*)) => {{
        let pattern = &$crate::pcre!($pattern $(, $($flags)*)?);
        pattern.with(|pat| pat.borrow_mut().$method($($args),*))
    }};
}

#[macro_export]
macro_rules! flags {
    ($($flags: ident)*) => {{
        #[allow(unused_mut)]
        let mut flags = ::enum_set::EnumSet::<::pcre::CompileOption>::new();
        $(
            $crate::flags_add!(flags, $flags);
        )*
        flags
    }};
}

#[macro_export]
macro_rules! flags_add {
    ($e: expr, x) => {
        $e.insert(::pcre::CompileOption::Extended);
    };
    ($e: expr, $offending_flag: ident) => {
        compile_error!(concat!("Unexpected flag: ", stringify!($offending_flag)));
    };
}

#[cfg(test)]
mod test {
    use pcre::CompileOption::*;

    #[test]
    fn test_macro() {
        let flags = flags!(x);
        assert_eq!(flags.iter().len(), 1);
        assert!(matches!(flags.iter().next(), Some(Extended)));

        let flags = flags!(x x);
        assert_eq!(flags.iter().len(), 1);
        assert!(matches!(flags.iter().next(), Some(Extended)));

        assert_eq!(flags!().iter().len(), 0);
    }

    #[test]
    fn test_regex() {
        let a = pcre!(r"\w+" => exec("   test   "));
        assert_eq!(a.map(|x| x.group(0)), Some("test"));

        let a = pcre!(r"
            \w+  # This has a comment
        ", x => exec("   test   "));
        assert_eq!(a.map(|x| x.group(0)), Some("test"));
    }
}