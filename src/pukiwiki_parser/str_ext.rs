use std::{borrow::Cow, ops::Range};

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

pub fn find_iter_str<'a>(
    haystack: &'a str,
    needle: &'a str,
) -> impl Iterator<Item = Range<usize>> + 'a + Clone {
    std::iter::successors(Some(0..0), move |r| {
        let i = r.end;
        haystack[i..]
            .find(needle)
            .map(|s| i + s..i + s + needle.len())
    })
    .skip(1)
}

pub fn find_iter_char(
    haystack: &str,
    needle: char,
) -> impl Iterator<Item = Range<usize>> + '_ + Clone {
    std::iter::successors(Some(0..0), move |r| {
        let i = r.end;
        haystack[i..]
            .find(needle)
            .map(|s| i + s..i + s + needle.len_utf8())
    })
    .skip(1)
}

pub fn find_iter_char_any<'a>(
    haystack: &'a str,
    needles: &'a [char],
) -> impl Iterator<Item = Range<usize>> + 'a + Clone {
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
