/// - This string does not contain `\n`.
/// - In this string, newline is represented by `\r`.
/// - It ends with `\r`.
pub struct PreprocessedString(String);
impl FromIterator<char> for PreprocessedString {
    fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
        let mapper = |c| match c {
            '\n' => Some('\r'),
            '\r' => None,
            c => Some(c),
        };
        let mut s: String = iter.into_iter().filter_map(mapper).collect();
        if !s.ends_with('\r') {
            s.push('\r');
        }
        Self(s)
    }
}
impl AsRef<str> for PreprocessedString {
    fn as_ref(&self) -> &str {
        &self.0
    }
}
