use std::borrow::Cow;

use either::*;
use itertools::Itertools;
use regex::Regex;

use crate::{
    either_ext::{into_common_2, EitherExt},
    my_itertools::MyItertools,
    regex,
};

pub struct Config {
    disable_multiline_plugin: bool,
    preformat_ltrim: bool,
}
impl Default for Config {
    fn default() -> Self {
        // Default value in pukiwiki.ini.php
        Self {
            disable_multiline_plugin: true,
            preformat_ltrim: true,
        }
    }
}
impl Config {
    pub fn taiko_wiki() -> Self {
        Self {
            disable_multiline_plugin: false,
            ..Default::default()
        }
    }
}

type FactoryInlineRet<'a> = Either<Paragraph<'a>, Inline<'a>>;
fn factory_inline(text: Cow<str>) -> FactoryInlineRet {
    if let Some(text) = strip_prefix_cow(&text, '~') {
        Left(Paragraph::new(text, ()))
    } else {
        Right(Inline::new(text))
    }
}

fn factory_dlist(text: &str) -> Either<DList, FactoryInlineRet> {
    if let Some((a, b)) = text.split_once('|') {
        Left(DList::new(a, b))
    } else {
        Right(factory_inline(text.into()))
    }
}

fn factory_table<'a>(text: &'a str, config: &Config) -> Either<Table<'a>, FactoryInlineRet<'a>> {
    if let Some(out) = regex!(r"^\|(.+)\|([hHfFcC]?)$").captures(text) {
        let kind = match &out[2] {
            "h" | "H" => TableRowKind::Header,
            "f" | "F" => TableRowKind::Footer,
            "c" | "C" => TableRowKind::Formatter,
            "" => TableRowKind::Nothing,
            _ => unreachable!("Guarded by regex"),
        };
        Left(Table::new(out.get(1).unwrap().as_str(), kind, config))
    } else {
        Right(factory_inline(text.into()))
    }
}

/// panics if text is empty
fn factory_ytable(text: &str) -> Either<YTable, FactoryInlineRet> {
    if text == "," {
        Right(factory_inline(text.into()))
    } else {
        Left(YTable::new(text[1..].split(',')))
    }
}

fn factory_div<'a>(
    text: &'a str,
    remaining_lines: impl IntoIterator<Item = &'a str>,
    disable_multiline_plugin: bool,
) -> Either<Div, Paragraph> {
    #[allow(clippy::collapsible_else_if)]
    if disable_multiline_plugin {
        if let Some(captures) = regex!(r"^\#([^\(]+)(?:\((.*)\))?").captures(text) {
            let name = captures.get(1).unwrap().as_str();
            let args = captures.get(2).map(|x| x.as_str());
            if exist_plugin_convert(name) {
                return Left(Div::new(name, args, vec![]));
            }
        }
    } else {
        if let Some(captures) = regex!(r"^#([^\(\{]+)(?:\(([^\r]*)\))?(\{*)").captures(text) {
            let name = captures.get(1).unwrap().as_str();
            if exist_plugin_convert(name) {
                let args = captures.get(2).map(|x| x.as_str());
                let brace_len = captures[3].len();
                if brace_len == 0 {
                    return Left(Div::new(name, args, vec![]));
                } else {
                    // TODO only if the last line starts with the same numeber of }'s
                    return Left(Div::new(name, args, remaining_lines));
                }
            }
        }
    }
    // TODO: is this the most efficient way?
    let text = remaining_lines
        .into_iter()
        .fold(text.to_owned(), |mut x, y| {
            x += y;
            x
        });
    Right(Paragraph::new(text.into(), ()))
}
fn exist_plugin_convert(_plugin_name: &str) -> bool {
    true
}

// Inline elements
#[derive(Debug)]
pub struct Inline<'a> {
    src: Cow<'a, str>,
}
impl<'a> Inline<'a> {
    fn new(text: Cow<'a, str>) -> Self {
        let text = if text.starts_with('\n') {
            trim_cow(&text)
        } else {
            // TODO: trim??
            make_link(text)
        };
        Self { src: text }
    }
}

// Paragraph: blank-line-separated sentences
#[derive(Debug)]
pub struct Paragraph<'a> {
    param: (),
    text: Option<Inline<'a>>,
}
impl<'a> Paragraph<'a> {
    fn new(text: Cow<'a, str>, param: ()) -> Self {
        let text = text.is_empty().then(|| {
            let text = strip_prefix_cow(&text, '~').unwrap_or(text);
            // The original code:
            // - first checks if it starts with '~';
            // - if so, replace the first character with ' ' and pass it to Factory_Inline;
            // - if not, pass it to Factory_Inline as it is.
            // As a result, this execution of Factory_Inline always results in a call to Inline().
            // The first ' ' as a result of replacement is trimmed in the Inline constructor.
            Inline::new(text)
        });
        Self { param, text }
    }
}

#[derive(Debug)]
pub struct Heading<'a> {
    level: HeadingLevel,
    tag: Option<&'a str>,
    text: FactoryInlineRet<'a>,
}
#[derive(Debug)]
enum HeadingLevel {
    H2,
    H3,
    H4,
}
impl<'a> Heading<'a> {
    /// # panics
    /// If text does not start with `*`
    fn new(text: &'a str) -> Self {
        let (level, text) = strip_prefix_n(text, '*', 3);
        let level = match level {
            0 => panic!("Should call with a text starting with '*'"),
            1 => HeadingLevel::H2,
            2 => HeadingLevel::H3,
            3 => HeadingLevel::H4,
            _ => unreachable!("By the contract of strip_prefix_n"),
        };

        // The following part corresponds to `get_anchor` in original code
        // What the function does:
        // - passes text to make_heading:
        //   - to strip the first link of form [#tag] as the anchor (*), but
        //   - not to strip footnotes (as $strip = FALSE)
        // - returns the following three values:
        //   - the converted text without tag, succeeded by an anchor if a named tag exists
        //   - a "back to top" link for second heading or later
        //   - a generated automatic id
        //
        // To obtain only the structure of the document, what we need is just to do (*).

        let (text, tag) = match regex!(r"\[#([A-Za-z][\w-]+)\]").captures(text) {
            Some(capture) => {
                let entire = capture.get(0).unwrap();
                let tag = capture.get(1).unwrap();
                let text = String::from(&text[..entire.start()]) + &text[entire.end()..];
                (Cow::Owned(text), Some(tag.as_str()))
            }
            None => (Cow::Borrowed(text), None),
        };

        // insert to self
        let text = factory_inline(text);
        Self { level, tag, text }
    }
}

#[derive(Debug)]
pub struct HRule;

#[derive(Debug)]
pub struct List<'a> {
    kind: ListKind,
    level: usize,
    text: FactoryInlineRet<'a>,
}
#[derive(Clone, Copy, Debug)]
enum ListKind {
    Ordered,
    Unordered,
}
impl<'a> List<'a> {
    // # Panics
    // If `text` does not start with specific character determined by `kind`
    fn new(text: &'a str, kind: ListKind) -> Self {
        let strip_char = match kind {
            ListKind::Ordered => '+',
            ListKind::Unordered => '-',
        };
        let (level, text) = strip_prefix_n(text, strip_char, 3);
        assert!(level > 0);
        let text = factory_inline(text.into());
        Self { kind, level, text }
    }
}

#[derive(Debug)]
pub struct DList<'a> {
    level: usize,
    word: FactoryInlineRet<'a>,
    desc: Option<FactoryInlineRet<'a>>,
}
impl<'a> DList<'a> {
    // # Panics
    // If `word` does not start with `:`.
    fn new(word: &'a str, desc: &'a str) -> Self {
        let (level, word) = strip_prefix_n(word, ':', 3);
        assert!(level > 0);
        let word = factory_inline(word.into());
        let desc = (!desc.is_empty()).then(|| factory_inline(desc.into()));
        Self { word, desc, level }
    }
}

#[derive(Debug)]
pub struct BQuote<'a> {
    level: usize,
    kind: BQuoteKind,
    text: Option<FactoryInlineRet<'a>>,
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum BQuoteKind {
    Start,
    End,
}
impl<'a> BQuote<'a> {
    // # Panics
    // If `text` does not start with `<` or `>`.
    fn new(text: &'a str, kind: BQuoteKind) -> Self {
        let strip_char = match kind {
            BQuoteKind::Start => '+',
            BQuoteKind::End => '-',
        };
        let (level, text) = strip_prefix_n(text, strip_char, 3);
        assert!(level > 0);
        let text =
            (kind == BQuoteKind::Start || !text.is_empty()).then(|| factory_inline(text.into()));
        Self { kind, text, level }
    }
}

#[derive(Debug)]
enum TableCell<'a> {
    MergeRight,
    MergeAbove,
    Content(TableContent<'a>),
}
#[derive(Debug)]
struct TableContent<'a> {
    is_header: bool,
    child: TableContentChild<'a>,
}
#[derive(Debug)]
enum TableContentChild<'a> {
    Paragraph(Paragraph<'a>),
    Inline(Inline<'a>),
    Div(Div<'a>),
    Empty,
}
#[derive(Clone, Default, Debug)]
struct TableStyle<'s> {
    align: Option<Align>,
    color: Option<CssColor<'s>>,
    background_color: Option<CssColor<'s>>,
    size: Option<u64>,
    width: Option<f64>,
}
#[derive(Clone, Debug)]
struct CssColor<'s>(&'s str);
impl<'a> TableCell<'a> {
    fn new(mut text: &'a str, is_template: bool, config: &Config) -> Self {
        let mut style = TableStyle::default();
        loop {
            let captures = match regex!(
                r"^(?:(LEFT|CENTER|RIGHT)|(BG)?COLOR\(([#0-9a-zA-Z_]+)\)|SIZE\(([0-9]+)\)):(.*)$"
            )
            .captures(text)
            {
                None => break,
                Some(c) => c,
            };
            if let Some(align) = captures.get(1) {
                let align = match align.as_str() {
                    "LEFT" => Align::Left,
                    "CETNER" => Align::Center,
                    "RIGHT" => Align::Right,
                    _ => unreachable!("Guarded by regex"),
                };
                style.align = Some(align);
            } else if let Some(color) = captures.get(3) {
                let color = CssColor(color.as_str());
                match captures.get(2) {
                    Some(_) => style.background_color = Some(color),
                    None => style.color = Some(color),
                };
            } else if let Some(size) = captures.get(4) {
                // Nits: Doesn't parse size greater than 18446744073709551615, but who cares?
                style.size = size.as_str().parse().ok();
            } else {
                break;
            }
            text = captures.get(5).unwrap().as_str();
        }
        if is_template {
            if let Some(value) = as_numeric(text) {
                // Nits: The behavior is slightly different.
                // In this code, if it fails to parse, style.width will remain unset.
                // In original code, it is always marked as a non-empty value.
                style.width = Some(value);
            }
        }

        if text == ">" {
            TableCell::MergeRight
        } else if text == "~" {
            TableCell::MergeAbove
        } else {
            let (text, is_header) = match text.strip_prefix('~') {
                Some(text) => (text, true),
                None => (text, false),
            };
            let child = if text.starts_with('#') {
                match factory_div(text, [], config.disable_multiline_plugin) {
                    Left(div) => TableContentChild::Div(div),
                    Right(para) => match para.text {
                        Some(inline) => TableContentChild::Inline(inline),
                        None => TableContentChild::Empty,
                    },
                }
            } else {
                match factory_inline(text.into()) {
                    Left(para) => TableContentChild::Paragraph(para),
                    Right(inl) => TableContentChild::Inline(inl),
                }
            };
            TableCell::Content(TableContent { is_header, child })
        }
    }
}

fn as_numeric(val: &str) -> Option<f64> {
    // https://www.php.net/manual/en/language.types.numeric-strings.php
    // WHITESPACES      \s*
    // LNUM             [0-9]+
    // DNUM             ([0-9]*)[\.]{LNUM}) | ({LNUM}[\.][0-9]*)
    // EXPONENT_DNUM    (({LNUM} | {DNUM}) [eE][+-]? {LNUM})
    // INT_NUM_STRING   {WHITESPACES} [+-]? {LNUM} {WHITESPACES}
    // FLOAT_NUM_STRING {WHITESPACES} [+-]? ({DNUM} | {EXPONENT_DNUM}) {WHITESPACES}
    // NUM_STRING       ({INT_NUM_STRING} | {FLOAT_NUM_STRING})
    let val = val.trim_start();
    let is_int = regex!(r"[+-]?[0-9]+").is_match(val);
    let is_float = regex!(
        r"^[+-]?([0-9]*\.[0-9]+|[0-9]+\.[0-9]*|([0-9]+|[0-9]*\.[0-9]+|[0-9]+\.[0-9]*)[eE][+-][0-9]+)$"
    )
    .is_match(val);
    (is_int || is_float).then(|| val.parse().ok()).flatten()
}

#[derive(Debug)]
pub struct Table<'a> {
    cells: Vec<TableCell<'a>>,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
enum TableRowKind {
    Header,
    Footer,
    Formatter,
    Nothing,
}
impl<'a> Table<'a> {
    fn new(cells: &'a str, kind: TableRowKind, config: &Config) -> Self {
        let cells = cells
            .split('|')
            .map(|cell| TableCell::new(cell, kind == TableRowKind::Formatter, config))
            .collect();
        Self { cells }
    }
}

#[derive(Debug)]
struct YTableCell<'a> {
    align: Align,
    content: YTableContent<'a>,
}
#[derive(Debug)]
enum YTableContent<'a> {
    MergeRight,
    Content(Cow<'a, str>),
}
impl<'a> YTableCell<'a> {
    fn new(text: &'a str) -> Self {
        let align = match (
            text.starts_with(char::is_whitespace),
            text.ends_with(char::is_whitespace),
        ) {
            (true, true) => Align::Center,
            (true, false) => Align::Right,
            (false, _) => Align::Left,
        };
        let text = text.trim();
        let content = if text == "==" {
            YTableContent::MergeRight
        } else {
            #[allow(clippy::unit_arg)]
            YTableContent::Content(make_link(text.into()))
        };
        Self { align, content }
    }
}

#[derive(Debug)]
pub struct YTable<'a> {
    cells: Vec<YTableCell<'a>>,
}
impl<'a> YTable<'a> {
    fn new(elements: impl IntoIterator<Item = &'a str>) -> Self {
        let cells = elements
            .into_iter()
            .map(|text| YTableCell::new(text))
            .collect();
        Self { cells }
    }
}

// ' 'Space-beginning sentence
#[derive(Debug)]
pub struct Pre<'a> {
    text: &'a str,
}
impl<'a> Pre<'a> {
    fn new(text: &'a str, preformat_ltrim: bool) -> Self {
        let _text = if preformat_ltrim && text.starts_with(' ') {
            &text[1..]
        } else {
            text
        };
        Self { text }
    }
}

#[derive(Debug)]
pub struct Div<'a> {
    plugin_name: &'a str,
    args: Option<&'a str>,
    remaining_lines: Vec<&'a str>,
}
impl<'a> Div<'a> {
    fn new(
        plugin_name: &'a str,
        args: Option<&'a str>,
        remaining_lines: impl IntoIterator<Item = &'a str>,
    ) -> Self {
        let remaining_lines = remaining_lines.into_iter().collect_vec();
        Self {
            plugin_name,
            args,
            remaining_lines,
        }
    }
}

#[derive(Clone, Copy, Debug)]
pub enum Align {
    Left,
    Center,
    Right,
}

const NEWLINES: &[char] = &['\r', '\n'];

/// Unless notated, the semantic of the element is "add-to-last".
#[derive(Debug, derive_more::From)]
pub enum Element<'a> {
    Inline(Inline<'a>),       // a
    Paragraph(Paragraph<'a>), // a
    Heading(Heading<'a>),     // insert toplevel
    HRule(HRule),             // insert toplevel
    List(List<'a>),           // a
    DList(DList<'a>),         // a
    BQuote(BQuote<'a>),       // a
    Table(Table<'a>),         // a
    YTable(YTable<'a>),       // a
    Pre(Pre<'a>),             // a
    Div(Div<'a>),             // a
    Align(Align),
    Clear, // Insert toplevel
}

pub fn parse<'a>(config: &Config, lines: &'a str) -> Vec<Element<'a>> {
    let mut lines = lines.split('\n');
    let mut ret: Vec<Element> = vec![];
    while let Some(line) = lines.next() {
        let line = &line;

        // Escape comments
        if line.starts_with("//") {
            continue;
        }

        let align_candidates = {
            use Align::*;
            [("LEFT:", Left), ("CENTER:", Center), ("RIGHT:", Right)]
        };
        let line = if let Some((&align, line)) = align_candidates
            .iter()
            .find_map(|(pat, ret)| line.strip_prefix(pat).map(|rem| (ret, rem)))
        {
            ret.push(align.into());
            if line.is_empty() {
                continue;
            }
            line
        } else {
            line
        };

        let line = line.trim_end_matches(NEWLINES);

        // Empty
        if line.is_empty() {
            ret.push(Element::Clear);
            continue;
        }

        // Horizontal Rule
        if line.starts_with("----") {
            ret.push(HRule.into());
            continue;
        }

        // Multiline-enabled block plugin
        if !config.disable_multiline_plugin {
            if let Some(res) = regex!(r"^#[^{]+(\{\{+)\s*$").captures(line) {
                let regex = Regex::new(&format!(r"\}}{{{}}}", res[1].len())).unwrap();
                let remaining_lines = lines
                    .by_ref()
                    .map(|line| line.trim_end_matches(NEWLINES))
                    .take_until(move |line| regex.is_match(line));
                // In this case, the line will always processed by #factory_inline.
                // The last ~ is ignored because in original source it is converted to \r
                // which is indistinguishable from terminal newline (maybe)
                let res = factory_div(line, remaining_lines, config.disable_multiline_plugin)
                    .into_common();
                ret.push(res);
                // That's why we may skip remaining process in this loop
                continue;
            }
        }

        // Heading
        if line.starts_with('*') {
            ret.push(Heading::new(line).into());
            continue;
        }

        // Pre
        if line.starts_with(&[' ', '\t'][..]) {
            ret.push(Pre::new(line, config.preformat_ltrim).into());
            continue;
        }

        // Line Break
        let line = if let Some(line) = line.strip_suffix('~') {
            // $line = substr($line, 0, -1) . "\r";
            line
        } else {
            line
        };

        // Other Character
        let res = match line.chars().next().expect("line is non-empty") {
            '-' => List::new(line, ListKind::Unordered).into(),
            '+' => List::new(line, ListKind::Ordered).into(),
            '>' => BQuote::new(line, BQuoteKind::Start).into(),
            '<' => BQuote::new(line, BQuoteKind::End).into(),
            ':' => into_common_2(factory_dlist(line)),
            '|' => into_common_2(factory_table(line, config)),
            ',' => into_common_2(factory_ytable(line)),
            '#' => factory_div(line, [], config.disable_multiline_plugin).into_common(),
            _ => factory_inline(line.into()).into_common(),
        };
        ret.push(res);
    }
    ret
}

fn make_link(text: Cow<str>) -> Cow<str> {
    // TODO
    text
}

/// Strips `c` from `s` as much as possible, but at most `n` times.
/// Returns the number of time `c` was stripped, and the remaining string.
/// The `(returned value).0` is in the range of `0..=n`.
fn strip_prefix_n(s: &str, c: char, n: usize) -> (usize, &str) {
    std::iter::successors(Some(s), |s| s.strip_prefix(c))
        .enumerate()
        .take(n + 1)
        .last()
        .unwrap()
}

#[allow(clippy::ptr_arg)]
fn strip_prefix_cow<'a>(str: &Cow<'a, str>, prefix: char) -> Option<Cow<'a, str>> {
    match str {
        Cow::Owned(str) => str.strip_prefix(prefix).map(|s| s.to_owned().into()),
        Cow::Borrowed(str) => str.strip_prefix(prefix).map(|s| s.into()),
    }
}

#[allow(clippy::ptr_arg)]
fn trim_cow<'a>(str: &Cow<'a, str>) -> Cow<'a, str> {
    match str {
        Cow::Owned(str) => str.trim().to_owned().into(),
        Cow::Borrowed(str) => str.trim().into(),
    }
}

#[cfg(test)]
mod test {
    use crate::pukiwiki_parser::parser::as_numeric;

    #[test]
    fn test_strip_prefix_n() {
        use super::strip_prefix_n;

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

    #[test]
    fn test_as_numeric() {
        assert_eq!(as_numeric("100"), Some(100.0));
        assert_eq!(as_numeric("+100"), Some(100.0));
        assert_eq!(as_numeric("-100"), Some(-100.0));

        assert_eq!(as_numeric(" 100"), Some(100.0));
        assert_eq!(as_numeric("    +100"), Some(100.0));
        assert_eq!(as_numeric(" \t\t\n\r-100"), Some(-100.0));

        assert_eq!(as_numeric("  13."), Some(13.));
        assert_eq!(as_numeric(" +13.75"), Some(13.75));
        assert_eq!(as_numeric(" -.75"), Some(-0.75));

        assert_eq!(as_numeric(" 4e3"), Some(4e3));
        assert_eq!(as_numeric(" -2.e+3"), Some(-2e3));
        assert_eq!(as_numeric(" +2.e-1"), Some(2e-1));

        assert_eq!(as_numeric("."), None);
        assert_eq!(as_numeric(" 3 pigs"), None);
        assert_eq!(as_numeric("There are 5 dogs"), None);
        assert_eq!(as_numeric("5 years"), None);
    }
}
