#[macro_export]
macro_rules! pcre {
    ($pattern: expr $(, $($flags: ident)*)?) => {{
        use pcre::Pcre;
        thread_local! {
            static PATTERN: Pcre = {
                let flags = $crate::flags!($($($flags)*)?);
                Pcre::compile_with_options($pattern, &flags).unwrap()
            };
        }
        PATTERN
    }};

    ($pattern: expr $(, $($flags: ident)*)? => $method: ident($($args: expr),*)) => {{
        let pattern = &$crate::pcre!($pattern $(, $($flags)*)?);
        pattern.with(|pat| pat.$method($($args),*))
    }};

    ($pattern: expr $(, $($flags: ident)*)? => ($arg: expr).$method: ident) => {{
        let pattern = &$crate::pcre!($pattern $(, $($flags)*)?);
        pattern.with(|pat| $arg.$method(pat))
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

mod inner {
    use std::ops::Range;

    use pcre::Match;

    pub trait MatchExt {
        fn group_opt_helper<'a>(&self, n: usize) -> Option<(&'a str, Range<usize>)> where Self: 'a;

        fn group_opt<'a>(&self, n: usize) -> Option<&'a str> where Self: 'a {
            Some(self.group_opt_helper(n)?.0)
        }
    }

    impl<'s> MatchExt for Match<'s> {
        fn group_opt_helper<'a>(&self, n: usize) -> Option<(&'s str, Range<usize>)> where Self: 'a {
            let start = self.group_start(n);
            (!start > 0).then(|| (self.group(n), start..self.group_end(n)))
        }
    }

}
pub use inner::MatchExt;

#[cfg(test)]
mod tests {
    use pcre::{CompileOption::*, Pcre};

    use super::MatchExt;

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

    #[test]
    fn test_match_ext() {
        let s = "This is a test.";
        let x = {
            let regex = Pcre::compile(r"\w+(z)?").unwrap();
            let res = regex.exec(s).unwrap();
            assert_eq!(res.group_opt(0), Some("This"));
            assert_eq!(res.group_opt(1), None);
            res.group_opt(0)
        };
        assert_eq!(x, Some("This"));
    }
}
