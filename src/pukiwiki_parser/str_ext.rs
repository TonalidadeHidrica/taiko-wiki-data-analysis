use std::{
    borrow::Cow,
    ops::{Bound, Index, Range, RangeBounds},
};

use len_trait::{Empty, Len};

use crate::{range_slice::RangeSlice, regex_ext::iter::IndexOwned};

/// Strips `c` from `s` as much as possible, but at most `n` times.
/// Returns the number of time `c` was stripped, and the remaining string.
/// The `(returned value).0` is in the range of `0..=n`.
pub fn strip_prefix_n(s: &str, c: char, n: usize) -> (usize, &str) {
    std::iter::successors(Some(s), |s| s.strip_prefix(c))
        .enumerate()
        .take(n + 1)
        .last()
        .unwrap()
}

#[allow(clippy::ptr_arg)]
pub fn strip_prefix_cow<'a>(str: &Cow<'a, str>, prefix: char) -> Option<Cow<'a, str>> {
    match str {
        Cow::Owned(str) => str.strip_prefix(prefix).map(|s| s.to_owned().into()),
        Cow::Borrowed(str) => str.strip_prefix(prefix).map(|s| s.into()),
    }
}

#[allow(clippy::ptr_arg)]
pub fn trim_cow<'a>(str: &Cow<'a, str>) -> Cow<'a, str> {
    match str {
        Cow::Owned(str) => str.trim().to_owned().into(),
        Cow::Borrowed(str) => str.trim().into(),
    }
}

pub fn find_iter_str<'a, 'o>(
    haystack: impl Into<&'a str>,
    needle: &'a str,
) -> impl Iterator<Item = Range<usize>> + 'a + Clone {
    let haystack = haystack.into();
    std::iter::successors(Some(0..0), move |r| {
        let i = r.end;
        haystack[i..]
            .find(needle)
            .map(|s| i + s..i + s + needle.len())
    })
    .skip(1)
}

pub fn find_iter_char<'a>(
    haystack: impl Into<&'a str>,
    needle: char,
) -> impl Iterator<Item = Range<usize>> + 'a + Clone {
    let haystack = haystack.into();
    std::iter::successors(Some(0..0), move |r| {
        let i = r.end;
        haystack[i..]
            .find(needle)
            .map(|s| i + s..i + s + needle.len_utf8())
    })
    .skip(1)
}

pub fn find_iter_char_any<'a>(
    haystack: impl Into<&'a str>,
    needles: &'a [char],
) -> impl Iterator<Item = Range<usize>> + 'a + Clone {
    let haystack = haystack.into();
    std::iter::successors(Some(0..0), move |r| {
        let i = r.end;
        haystack[i..].find(needles).map(|s| {
            let s = i + s;
            let t = (s + 1..)
                .find(|&i| haystack.is_char_boundary(i))
                .expect("Always exists");
            s..t
        })
    })
    .skip(1)
}

//     left == "" => right == ""
// <=> left != "" || right == ""
#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub struct TwoStr<'a> {
    left: &'a str,
    right: &'a str,
}

/// Constructors
impl<'a> TwoStr<'a> {
    pub fn new(left: &'a str, right: &'a str) -> Self {
        match left {
            "" => right.into(),
            _ => Self { left, right }, // SAFETY: left is non-empty
        }
    }

    pub fn split_concat(str: &'a str, split_at: impl RangeSlice<usize>) -> Self {
        let &s = split_at.start_bound_slice().unwrap_or(&0);
        let &t = split_at.end_bound_slice().unwrap_or(&str.len());
        Self::new(&str[..s], &str[t..])
    }
}

impl<'a, T: AsRef<str> + 'a> From<T> for TwoStr<'a> {
    fn from(str: T) -> Self {
        Self {
            left: str.as_ref(),
            right: "",
        } // SAFETY: Right is empty
    }
}

fn into_range(range: impl RangeBounds<usize>, len: usize) -> Range<usize> {
    let start = match range.start_bound() {
        Bound::Included(&start) => start,
        Bound::Excluded(&start) => start
            .checked_add(1)
            .expect("Start bound caused an overflow"),
        Bound::Unbounded => 0,
    };

    let end = match range.end_bound() {
        Bound::Included(&end) => end.checked_add(1).expect("End bound caused an overflow"),
        Bound::Excluded(&end) => end,
        Bound::Unbounded => len,
    };

    start..end
}

impl<'a, R: RangeBounds<usize>> Index<R> for TwoStr<'a> {
    type Output = Self;

    fn index(&self, index: R) -> &Self::Output {
        let len_l = self.left.len();
        let len = len_l + self.right.len();
        let Range { start, end } = into_range(index, len);
        assert!(
            start <= end,
            "slice index starts at {} but ends at {}",
            start,
            end
        );
        assert!(
            end <= len,
            "range end index {} out of range for slice of length {}",
            end,
            len
        );
        &Self::new(
            &self.left[start.min(len_l)..end.min(len_l)],
            &self.right[start.max(len_l)..end.max(len_l)],
        )
    }
}

/// `str` counterparts
impl<'a> TwoStr<'a> {
    pub fn is_empty(self) -> bool {
        self.left.is_empty()
    }

    pub fn starts_with(self, prefix: char) -> bool {
        self.left.starts_with(prefix)
    }

    pub fn strip_prefix(self, prefix: impl TwoStrStrip) -> Option<Self> {
        prefix.strip_prefix(self)
    }

    pub fn trim(self) -> Self {
        self.trim_start().trim_end()
    }

    pub fn trim_start(self) -> Self {
        match self.left.trim_start() {
            "" => self.right.trim_start().into(),
            left => Self { left, ..self }, // SAFETY: left is non-empty
        }
    }

    pub fn trim_end(self) -> Self {
        match self.right.trim_end() {
            "" => self.left.trim_end().into(),
            right => Self { right, ..self }, // SAFETY: right is non-empty
        }
    }
}

impl<'a> IntoIterator for TwoStr<'a> {
    type Item = &'a str;
    type IntoIter = TwoStrIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        TwoStrIter(self, 0)
    }
}

pub trait TwoStrStrip {
    fn strip_prefix(self, str: TwoStr) -> Option<TwoStr>;
}

impl TwoStrStrip for char {
    fn strip_prefix(self, str: TwoStr) -> Option<TwoStr> {
        Some(TwoStr::new(str.left.strip_prefix(self)?, str.right))
    }
}

impl PartialEq<str> for TwoStr<'_> {
    fn eq(&self, other: &str) -> bool {
        self.left.len() + self.right.len() == other.len()
            && other.starts_with(self.left)
            && other.ends_with(self.right)
    }
}

#[derive(Clone)]
pub struct TwoStrIter<'a>(TwoStr<'a>, usize);

impl<'a> Iterator for TwoStrIter<'a> {
    type Item = &'a str;

    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match *[self.0.left, self.0.right].get(self.1)? {
            "" => None,
            s => {
                self.1 += 1;
                Some(s)
            }
        }
    }
}

/// - `'o` stands for the lifetime to the original str.
/// - `self.0 == self.1.left + self.1.right`
#[derive(Clone, Debug)]
pub struct TwoStrConcat<'o>(String, TwoStr<'o>);

impl<'a> TwoStr<'a> {
    pub fn into_concat(self) -> TwoStrConcat<'a> {
        TwoStrConcat(self.left.to_owned() + self.right, self)
    }
}

impl<'o> TwoStrConcat<'o> {
    pub fn as_concat_ref<'a>(&'a self) -> TwoStrConcatRef<'a, 'o> {
        TwoStrConcatRef(&self.0, self.1)
    }
}

/// - `'a` stands for the lifetime to the concat str.
/// - `'o` stands for the lifetime to the original str.
/// - `self.0 == self.1.left + self.1.right`
#[derive(Clone, Copy, Default)]
pub struct TwoStrConcatRef<'a, 'o>(&'a str, TwoStr<'o>);

impl<'a> From<&'a str> for TwoStrConcatRef<'a, 'a> {
    fn from(this: &'a str) -> Self {
        Self(this, this.into())
    }
}

impl<'a, 'o> From<TwoStrConcatRef<'a, 'o>> for &'a str {
    fn from(this: TwoStrConcatRef<'a, 'o>) -> Self {
        this.0
    }
}

impl<'a, 'o> From<TwoStrConcatRef<'a, 'o>> for TwoStr<'o> {
    fn from(this: TwoStrConcatRef<'a, 'o>) -> Self {
        this.1
    }
}

impl<'a, 'o> Empty for TwoStrConcatRef<'a, 'o> {
    fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl<'a, 'o> Len for TwoStrConcatRef<'a, 'o> {
    fn len(&self) -> usize {
        self.0.len()
    }
}

impl<'a, 'o> TwoStrConcatRef<'a, 'o> {
    pub fn pcre_matches<'r>(
        self,
        regex: &'r ::pcre::Pcre,
    ) -> self::pcre::MatchIterator<'r, 'a, 'o> {
        self::pcre::MatchIterator(regex.matches(self.0), self)
    }

    pub fn pcre_exec<'r>(self, regex: &'r ::pcre::Pcre) -> Option<self::pcre::Match<'a, 'o>> {
        Some(self::pcre::Match {
            m: regex.exec(self.0)?,
            str: self,
        })
    }

    pub fn regex_find_iter<'r>(
        self,
        regex: &'r ::regex::Regex,
    ) -> self::regex::Matches<'r, 'a, 'o> {
        self::regex::Matches {
            it: regex.find_iter(self.0),
            str: self,
        }
    }

    pub fn regex_captures<'r>(
        self,
        regex: &'r ::regex::Regex,
    ) -> Option<self::regex::Captures<'a, 'o>> {
        Some(self::regex::Captures {
            captures: regex.captures(self.0)?,
            str: self,
        })
    }

    pub fn regex_captures_iter<'r>(
        self,
        regex: &'r ::regex::Regex,
    ) -> self::regex::CaptureMatches<'r, 'a, 'o> {
        self::regex::CaptureMatches {
            it: regex.captures_iter(self.0),
            str: self,
        }
    }

    pub fn regex_is_match(&self, regex: &::regex::Regex) -> bool {
        regex.is_match(self.0)
    }
}

impl<'a, 'o, R: RangeBounds<usize>> Index<R> for TwoStrConcatRef<'a, 'o> {
    type Output = Self;

    fn index(&self, index: R) -> &Self::Output {
        let range = into_range(index, self.len());
        &Self(&self.0[range], self.1[range])
    }
}
impl<'a, 'o, I> IndexOwned<I> for TwoStrConcatRef<'a, 'o>
where
    Self: Index<I>,
    <Self as Index<I>>::Output: Sized,
{
    type Output = <Self as Index<I>>::Output;

    fn index_owned(self, index: I) -> Self::Output {
        self[index]
    }
}

pub mod pcre {
    use crate::regex_ext::{iter::MatchLike, pcre::MatchExt};

    use super::TwoStrConcatRef;

    pub struct MatchIterator<'r, 'a, 'o>(
        pub(super) ::pcre::MatchIterator<'a, 'r>,
        pub(super) TwoStrConcatRef<'a, 'o>,
    );

    impl<'r, 'a, 'o> Iterator for MatchIterator<'r, 'a, 'o> {
        type Item = self::Match<'a, 'o>;

        fn next(&mut self) -> Option<Self::Item> {
            Some(Match {
                m: self.0.next()?,
                str: self.1,
            })
        }
    }

    pub struct Match<'a, 'o> {
        m: ::pcre::Match<'a>,
        str: TwoStrConcatRef<'a, 'o>,
    }

    impl<'a, 'o> MatchLike for Match<'a, 'o> {
        fn start_pos(&self) -> usize {
            self.m.start_pos()
        }

        fn end_pos(&self) -> usize {
            self.m.end_pos()
        }
    }

    impl<'a, 'o> Match<'a, 'o> {
        pub fn group(self, n: usize) -> TwoStrConcatRef<'a, 'o> {
            self.str[self.m.group_start(n)..self.m.group_end(n)]
        }

        pub fn group_opt(self, n: usize) -> Option<TwoStrConcatRef<'a, 'o>> {
            let (group, range) = self.m.group_opt_helper(n)?;
            Some(self.str[range])
        }
    }
}

pub mod regex {
    use std::ops::Range;

    use crate::regex_ext::iter::MatchLike;

    use super::TwoStrConcatRef;

    pub struct Matches<'r, 'a, 'o> {
        pub(super) it: ::regex::Matches<'r, 'a>,
        pub(super) str: TwoStrConcatRef<'a, 'o>,
    }

    impl<'r, 'a, 'o> Iterator for Matches<'r, 'a, 'o> {
        type Item = Match<'a, 'o>;

        fn next(&mut self) -> Option<Self::Item> {
            Some(Match {
                m: self.it.next()?,
                str: self.str,
            })
        }
    }

    pub struct CaptureMatches<'r, 'a, 'o> {
        pub(super) it: ::regex::CaptureMatches<'r, 'a>,
        pub(super) str: TwoStrConcatRef<'a, 'o>,
    }

    impl<'r, 'a, 'o> Iterator for CaptureMatches<'r, 'a, 'o> {
        type Item = Captures<'a, 'o>;

        fn next(&mut self) -> Option<Self::Item> {
            Some(Captures {
                captures: self.it.next()?,
                str: self.str,
            })
        }
    }

    pub struct Captures<'a, 'o> {
        captures: ::regex::Captures<'a>,
        str: TwoStrConcatRef<'a, 'o>,
    }

    impl<'a, 'o> MatchLike for Captures<'a, 'o> {
        fn start_pos(&self) -> usize {
            self.captures.start_pos()
        }

        fn end_pos(&self) -> usize {
            self.captures.end_pos()
        }
    }

    impl<'a, 'o> Captures<'a, 'o> {
        pub fn get(&self, i: usize) -> Option<Match<'a, 'o>> {
            Some(Match {
                m: self.captures.get(i)?,
                str: self.str,
            })
        }

        pub fn name<'n>(&self, name: &'n str) -> Option<Match<'a, 'o>> {
            Some(Match {
                m: self.captures.name(name)?,
                str: self.str,
            })
        }
    }

    pub struct Match<'a, 'o> {
        m: ::regex::Match<'a>,
        str: TwoStrConcatRef<'a, 'o>,
    }

    impl<'a, 'o> Match<'a, 'o> {
        pub fn range(&self) -> Range<usize> {
            self.m.range()
        }

        pub fn as_str(&self) -> TwoStrConcatRef<'a, 'o> {
            self.str[self.m.range()]
        }
    }
}

#[cfg(test)]
mod tests {
    use super::strip_prefix_n;

    #[test]
    fn test_strip_prefix_n() {
        assert_eq!(strip_prefix_n("bcde", 'a', 0), (0, "bcde"));
        assert_eq!(strip_prefix_n("abcde", 'a', 0), (0, "abcde"));
        assert_eq!(strip_prefix_n("aabcde", 'a', 0), (0, "aabcde"));

        assert_eq!(strip_prefix_n("bcde", 'a', 3), (0, "bcde"));
        assert_eq!(strip_prefix_n("abcde", 'a', 3), (1, "bcde"));
        assert_eq!(strip_prefix_n("aabcde", 'a', 3), (2, "bcde"));
        assert_eq!(strip_prefix_n("aaabcde", 'a', 3), (3, "bcde"));
        assert_eq!(strip_prefix_n("aaaabcde", 'a', 3), (3, "abcde"));
        assert_eq!(strip_prefix_n("aaaaabcde", 'a', 3), (3, "aabcde"));
    }
}
