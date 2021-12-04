use std::{borrow::Cow, collections::HashMap, iter::Peekable, ops::Range};

use either::*;
use entities::{Entity, ENTITIES};
use itertools::{any, zip, Itertools};
use loop_unwrap::unwrap_break;
use once_cell::sync::Lazy;
use regex::{Captures, Regex};

use crate::{
    either_ext::{into_common_2, EitherExt},
    my_itertools::{MyItertools, PeekableExt},
    pcre, regex,
    regex_ext::{
        iter::{MatchComponent, MatchIterator, MatchLike},
        pcre::MatchExt,
    },
};

pub struct Config {
    disable_multiline_plugin: bool,
    disable_inline_image_from_uri: bool,
    preformat_ltrim: bool,
}
impl Default for Config {
    fn default() -> Self {
        // Default value in pukiwiki.ini.php
        Self {
            disable_multiline_plugin: true,
            disable_inline_image_from_uri: false,
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
fn factory_inline<'a>(text: Cow<'a, str>, config: &Config) -> FactoryInlineRet<'a> {
    if let Some(text) = strip_prefix_cow(&text, '~') {
        Left(Paragraph::new(text, (), config))
    } else {
        Right(Inline::new(text, config))
    }
}

fn factory_dlist<'a>(text: &'a str, config: &Config) -> Either<DList<'a>, FactoryInlineRet<'a>> {
    if let Some((a, b)) = text.split_once('|') {
        Left(DList::new(a, b, config))
    } else {
        Right(factory_inline(text.into(), config))
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
        Right(factory_inline(text.into(), config))
    }
}

/// panics if text is empty
fn factory_ytable<'a>(text: &'a str, config: &Config) -> Either<YTable<'a>, FactoryInlineRet<'a>> {
    if text == "," {
        Right(factory_inline(text.into(), config))
    } else {
        Left(YTable::new(text[1..].split(','), config))
    }
}

fn factory_div<'a>(
    text: &'a str,
    remaining_lines: impl IntoIterator<Item = &'a str>,
    disable_multiline_plugin: bool,
    config: &Config,
) -> Either<Div<'a>, Paragraph<'a>> {
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
                    // TODO [compl] only if the last line starts with the same numeber of }'s
                    return Left(Div::new(name, args, remaining_lines));
                }
            }
        }
    }
    // TODO: [perf] is this the most efficient way?
    let text = remaining_lines
        .into_iter()
        .fold(text.to_owned(), |mut x, y| {
            x += y;
            x
        });
    Right(Paragraph::new(text.into(), (), config))
}
fn exist_plugin_convert(_plugin_name: &str) -> bool {
    true
}

// Inline elements
#[derive(Debug)]
pub struct Inline<'a> {
    // src: Cow<'a, str>,
    elements: Vec<InlineElement>,
    /// Maybe we are going to use this lifetime afterwards
    _phantom: std::marker::PhantomData<fn() -> &'a ()>,
}
impl<'a> Inline<'a> {
    fn new(text: Cow<'a, str>, config: &Config) -> Self {
        let elements = if text.starts_with('\n') {
            vec![trim_cow(&text).into_owned().into()]
        } else {
            // TODO: [compl] trim??
            make_link(text, config)
        };
        Self {
            _phantom: Default::default(),
            elements,
        }
    }
}

// Paragraph: blank-line-separated sentences
#[derive(Debug)]
pub struct Paragraph<'a> {
    param: (),
    text: Option<Inline<'a>>,
}
impl<'a> Paragraph<'a> {
    fn new(text: Cow<'a, str>, param: (), config: &Config) -> Self {
        let text = text.is_empty().then(|| {
            let text = strip_prefix_cow(&text, '~').unwrap_or(text);
            // The original code:
            // - first checks if it starts with '~';
            // - if so, replace the first character with ' ' and pass it to Factory_Inline;
            // - if not, pass it to Factory_Inline as it is.
            // As a result, this execution of Factory_Inline always results in a call to Inline().
            // The first ' ' as a result of replacement is trimmed in the Inline constructor.
            Inline::new(text, config)
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
    fn new(text: &'a str, config: &Config) -> Self {
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
        let text = factory_inline(text, config);
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
    fn new(text: &'a str, kind: ListKind, config: &Config) -> Self {
        let strip_char = match kind {
            ListKind::Ordered => '+',
            ListKind::Unordered => '-',
        };
        let (level, text) = strip_prefix_n(text, strip_char, 3);
        assert!(level > 0);
        let text = factory_inline(text.into(), config);
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
    fn new(word: &'a str, desc: &'a str, config: &Config) -> Self {
        let (level, word) = strip_prefix_n(word, ':', 3);
        assert!(level > 0);
        let word = factory_inline(word.into(), config);
        let desc = (!desc.is_empty()).then(|| factory_inline(desc.into(), config));
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
    fn new(text: &'a str, kind: BQuoteKind, config: &Config) -> Self {
        let strip_char = match kind {
            BQuoteKind::Start => '>',
            BQuoteKind::End => '<',
        };
        let (level, text) = strip_prefix_n(text, strip_char, 3);
        assert!(level > 0);
        let text = (kind == BQuoteKind::Start || !text.is_empty())
            .then(|| factory_inline(text.into(), config));
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
                    "CENTER" => Align::Center,
                    "RIGHT" => Align::Right,
                    _ => unreachable!("Guarded by regex: {:?}", align),
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
                match factory_div(text, [], config.disable_multiline_plugin, config) {
                    Left(div) => TableContentChild::Div(div),
                    Right(para) => match para.text {
                        Some(inline) => TableContentChild::Inline(inline),
                        None => TableContentChild::Empty,
                    },
                }
            } else {
                match factory_inline(text.into(), config) {
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
    Content(Vec<InlineElement>, std::marker::PhantomData<fn() -> &'a ()>),
}
impl<'a> YTableCell<'a> {
    fn new(text: &'a str, config: &Config) -> Self {
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
            YTableContent::Content(make_link(text.into(), config), Default::default())
        };
        Self { align, content }
    }
}

#[derive(Debug)]
pub struct YTable<'a> {
    cells: Vec<YTableCell<'a>>,
}
impl<'a> YTable<'a> {
    fn new(elements: impl IntoIterator<Item = &'a str>, config: &Config) -> Self {
        let cells = elements
            .into_iter()
            .map(|text| YTableCell::new(text, config))
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

// "a" stands for "add-to-last".
#[derive(Debug, derive_more::From)]
pub enum Element<'a> {
    Inline(Inline<'a>),       // a
    Paragraph(Paragraph<'a>), // a
    Heading(Heading<'a>),     // insert to toplevel
    HRule(HRule),             // insert to toplevel
    List(List<'a>),           // a
    DList(DList<'a>),         // a
    BQuote(BQuote<'a>),       // a
    Table(Table<'a>),         // a
    YTable(YTable<'a>),       // a
    Pre(Pre<'a>),             // a
    Div(Div<'a>),             // a
    Align(Align),             // (Affect to the next element)
    NewLine,                  // a
    Clear,                    // insert to toplevel
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
                let res = factory_div(
                    line,
                    remaining_lines,
                    config.disable_multiline_plugin,
                    config,
                );
                ret.push(res.into_common());
                // That's why we may skip remaining process in this loop
                continue;
            }
        }

        // Heading
        if line.starts_with('*') {
            ret.push(Heading::new(line, config).into());
            continue;
        }

        // Pre
        if line.starts_with(&[' ', '\t'][..]) {
            ret.push(Pre::new(line, config.preformat_ltrim).into());
            continue;
        }

        // Line Break
        let (line, has_newline) = if let Some(line) = line.strip_suffix('~') {
            (line, true)
        } else {
            (line, false)
        };

        // Other Character
        let res = match line.chars().next() {
            Some('-') => List::new(line, ListKind::Unordered, config).into(),
            Some('+') => List::new(line, ListKind::Ordered, config).into(),
            Some('>') => BQuote::new(line, BQuoteKind::Start, config).into(),
            Some('<') => BQuote::new(line, BQuoteKind::End, config).into(),
            Some(':') => into_common_2(factory_dlist(line, config)),
            Some('|') => into_common_2(factory_table(line, config)),
            Some(',') => into_common_2(factory_ytable(line, config)),
            Some('#') => {
                factory_div(line, [], config.disable_multiline_plugin, config).into_common()
            }
            _ => factory_inline(line.into(), config).into_common(),
        };
        ret.push(res);

        if has_newline {
            ret.push(Element::NewLine);
        }
    }
    ret
}

#[derive(Debug, derive_more::From)]
pub enum InlineElement {
    InlineToken(InlineToken),
    InlinePlugin(InlinePlugin),
    Footnote(Footnote),
    Link(Link),
    InterWikiUrl(InterWikiUrl),
    MailTo(MailTo),
    Image(Image),
    InterWikiNameUrl(InterWikiNameUrl),
    PageLink(PageLink),
}
#[derive(Debug)]
pub struct InlinePlugin {
    plugin_name: String,
    parameter: Option<String>,
    body: Option<Vec<InlineElement>>,
}
#[derive(Debug)]
pub struct Footnote {
    contents: Vec<InlineElement>,
}
#[derive(Debug)]
pub struct Link {
    url: String,
    caption: Vec<InlineElement>,
}
#[derive(Debug)]
pub struct InterWikiUrl {
    url: String,
    caption: Vec<InlineElement>,
}
#[derive(Debug)]
pub struct MailTo {
    address: String,
    caption: Vec<InlineElement>,
}
#[derive(Debug)]
pub struct Image {
    src: String,
    alt: String,
}
#[derive(Debug)]
pub struct InterWikiNameUrl {
    destination: InterWikiDestination,
    anchor: Option<String>,
    name: String,
    caption: Vec<InlineElement>,
}
#[derive(Debug)]
pub enum InterWikiDestination {
    Myself {
        page_name: String,
    },
    Other {
        base_url: Option<String>, // If not exists, it indicates the wiki itself
        param: String,
        opt: String,
    },
}

// Temporary
impl From<String> for InlineElement {
    fn from(string: String) -> Self {
        InlineElement::InlineToken(InlineToken::Str(string))
    }
}

fn make_link(text: Cow<str>, config: &Config) -> Vec<InlineElement> {
    let text = text.into_owned();
    pcre!(
        r#"""
            ( # Link_plugin (1)
                &
                (      # (1) plain
                 (\w+) # (2) plugin name
                 (?:
                  \(
                   ((?:(?!\)[;{]).)*) # (3) parameter
                  \)
                 )?
                )
                (?:
                 \{
                  ((?:(?R)|(?!};).)*) # (4) body
                 \}
                )?
                ;
            )
        |
            ( # Link_note (6)
                \(\(
                 ((?:(?R)|(?!\)\)).)*) # (1) note body
                \)\)
            )
        |
            ( # Link_url (8)
                (\[\[             # (1) open bracket
                 ((?:(?!\]\]).)+) # (2) alias
                 (?:>|:)
                )?
                (                 # (3) url
                 (?:(?:https?|ftp|news):\/\/|mailto:)[\w\/\@$()!?&%#:;.,~'=*+-]+
                )
                (?(9)\]\])      # close bracket
            )
        |
            ( # Link_url_interwiki (12)
                \[       # open bracket
                (        # (1) url
                 (?:(?:https?|ftp|news):\/\/|\.\.?\/)[!~*'();\/?:\@&=+$,%#\w.-]*
                )
                \s
                ([^\]]+) # (2) alias
                \]       # close bracket
            )
        |
            ( # Link_mailto (15)
                (?:
                 \[\[
                 ((?:(?!\]\]).)+)(?:>|:)  # (1) alias
                )?
                ([\w.-]+@[\w-]+\.[\w.-]+) # (2) mailto
                (?(16)\]\])              # close bracket if (1)
            )
        |
            ( # Link_interwikiname (18)
                \[\[                  # open bracket
                (?:
                 ((?:(?!\]\]).)+)>    # (1) alias
                )?
                (\[\[)?               # (2) open bracket
                ((?:(?!\s|:|\]\]).)+) # (3) InterWiki
                (?<! > | >\[\[ )      # not '>' or '>[['
                :                     # separator
                (                     # (4) param
                 (\[\[)?              # (5) open bracket
                 (?:(?!>|\]\]).)+
                 (?(23)\]\])         # close bracket if (5)
                )
                (?(20)\]\])          # close bracket if (2)
                \]\]                  # close bracket
            )
        |
            ( # Link_bracketname (24)
                \[\[                     # Open bracket
                (?:((?:(?!\]\]).)+)>)?   # (1) Alias
                (\[\[)?                  # (2) Open bracket
                (                        # (3) PageName
                 (?:(?:[A-Z][a-z]+){2,}(?!\w))
                 |
                 (?:(?!\s):?[^\r\n\t\f\[\]<>#&":]+:?(?<!\s))
                )?
                (\#(?:[a-zA-Z][\w-]*)?)? # (4) Anchor
                (?(26)\]\])             # Close bracket if (2)
                \]\]                     # Close bracket
            )
        |
            ( # Link_wikiname (29)
                ((?:[A-Z][a-z]+){2,}(?!\w))
            )
        """#,
        x
    )
    .with(|pattern| {
        let mut res: Vec<InlineElement> = Vec::new();
        for groups in pattern.matches(&text).match_components(&text) {
            let groups = match groups {
                MatchComponent::Match(m) => m,
                MatchComponent::Between(str) => {
                    res.push(str.to_owned().into());
                    continue;
                }
            };
            if let Some(group) = groups.group_opt(1) {
                // Link_plugin
                let plugin_name = groups.group(2).to_owned();
                let parameter = groups.group_opt(3).map(str::to_owned);
                let body = groups.group_opt(4).map(|s| make_link(s.into(), config));
                if exist_plugin_inline(&plugin_name) {
                    let parsed = InlinePlugin {
                        plugin_name,
                        parameter,
                        body,
                    };
                    res.push(parsed.into());
                } else {
                    res.extend(make_line_rules(group).map(InlineElement::InlineToken));
                }
            } else if groups.group_opt(6).is_some() {
                // Link_note
                let body = groups.group(7).to_owned();
                let contents = make_link(body.into(), config);
                res.push(Footnote { contents }.into());
            } else if groups.group_opt(8).is_some() {
                // Link_url
                let alias = groups.group_opt(10);
                let url = groups.group(11).to_owned();
                let caption =
                    alias.map_or_else(Vec::new, |alias| parse_alias(url.to_owned(), alias, config));
                res.push(Link { caption, url }.into());
            } else if groups.group_opt(12).is_some() {
                // Link_url_interwiki
                let url = groups.group(13).to_owned();
                let alias = groups.group(14);
                let caption = parse_alias(url.to_owned(), alias, config);
                res.push(InterWikiUrl { url, caption }.into());
            } else if groups.group_opt(15).is_some() {
                // Link_mailto
                let alias = groups.group_opt(16);
                let mailto = groups.group(17).to_owned();
                let caption = alias.map_or_else(Vec::new, |alias| {
                    parse_alias(mailto.to_owned(), alias, config)
                });
                let parsed = MailTo {
                    caption,
                    address: mailto,
                };
                res.push(parsed.into());
            } else if groups.group_opt(18).is_some() {
                // Link_interwikiname
                let alias = groups.group_opt(19);
                let name = groups.group(21).to_owned();
                let param = groups.group(22).to_owned();
                let (param, anchor) = if let Some(captures) =
                    regex!(r"^([^#]+)(#[A-Za-z][\w-]*)$").captures(&param)
                {
                    (
                        captures.get(1).unwrap().as_str().to_owned(),
                        captures.get(2).map(|x| x.as_str()),
                    )
                } else {
                    (param, None)
                };
                let caption = alias.map_or_else(Vec::new, |alias| {
                    parse_alias(name.clone() + ":" + &param, alias, config)
                });
                let destination = get_interwiki_url(&name, &param)
                    .unwrap_or(InterWikiDestination::Myself { page_name: param });
                let parsed = InterWikiNameUrl {
                    destination,
                    anchor: anchor.map(str::to_owned),
                    name,
                    caption,
                };
                res.push(parsed.into());
            } else if let Some(group) = groups.group_opt(24) {
                // Link_bracketname
                let alias = groups.group_opt(25).map(str::to_owned);
                let name = groups.group_opt(27).map(str::to_owned);
                let anchor = groups.group_opt(28).map(str::to_owned);
                let name_ref = name.as_deref().unwrap_or("");
                let name_is_empty = name.as_ref().map_or(true, |s| s.is_empty());
                if name_is_empty && anchor.as_ref().map_or(true, |s| s.is_empty()) {
                    res.extend(make_line_rules(group).map(InlineElement::InlineToken));
                } else if name_is_empty
                    // not wikiname
                    || pcre!(r"^(?:[A-Z][a-z]+){2,}(?!\w)$" => exec(name_ref)).is_none()
                {
                    // TODO [compl] if the page does not exist, it should return plain text instead
                    // TODO [compl] update name with absolute name
                    let alias = if alias.as_ref().map_or(true, |x| x.is_empty()) {
                        if let Some(anchor) = &anchor {
                            Some(name.as_ref().cloned().unwrap_or_default() + anchor)
                        } else {
                            name.clone()
                        }
                    } else {
                        alias
                    };
                    let contents = alias.map_or_else(Vec::new, |alias| {
                        parse_alias(name.as_ref().cloned().unwrap_or_default(), &alias, config)
                    });
                    res.push(make_page_link(name, contents, anchor, false).into());
                }
            } else if groups.group_opt(29).is_some() {
                // Link_wikiname
                let wikiname = groups.group(30).to_owned();
                res.push(
                    make_page_link(wikiname.clone(), vec![wikiname.into()], None, false).into(),
                );
            } else {
                unreachable!("The regex has either of these groups")
            };
        }
        res
    })
}
fn exist_plugin_inline(_plugin_name: &str) -> bool {
    true
}
fn parse_alias(name: String, alias: &str, config: &Config) -> Vec<InlineElement> {
    if config.disable_inline_image_from_uri
        && is_url(alias)
        && regex!(r"\.(gif|png|jpe?g)$").is_match(alias)
    {
        let parsed = Image {
            src: alias.to_owned(),
            alt: name,
        };
        vec![parsed.into()]
    } else if !alias.is_empty() {
        // $page is an external information, so we don't
        // TODO make_link converts plugin, but here it shouldn't
        // TODO make_line_rules
        make_link(alias.into(), config)
    } else {
        vec![]
    }
}
fn is_url(str: &str) -> bool {
    // assume $only_http = FALSE
    // Unnecessary escape sequences are removed
    regex!(r"^(https?|ftp|news)(://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]*)$").is_match(str)
}
fn get_interwiki_url(_name: &str, _param: &str) -> Option<InterWikiDestination> {
    // TODO: [prio:low] implement
    // Note that taiko wiki does not have contents in InterWikiName,
    // so this does not affect to the parse result.
    None
}

#[derive(Debug, derive_more::From)]
pub enum InlineToken {
    Str(String),
    SpecialChar(SpecialChar),
    NewLine,
    FaceMark(FaceMark),
    PushButton(PushButton),
    MobileEmoji(MobileEmoji),
    StyleSpecifierStart(StyleSpecifierStart),
    #[from(ignore)]
    StyleStart(StyleKind),
    #[from(ignore)]
    StyleEnd(StyleKind),
}
#[derive(Debug, derive_more::From)]
pub enum SpecialChar {
    NamedEntity(&'static Entity),
    Char(char),
    ReplacementCharacter,
}
#[derive(Debug)]
pub enum FaceMark {
    Smile,
    BigSmile,
    Huh,
    Oh,
    Wink,
    Sad,
    Heart,
    Worried,
}
#[derive(Debug)]
pub enum PushButton {
    Pb0,
    Pb1,
    Pb2,
    Pb3,
    Pb4,
    Pb5,
    Pb6,
    Pb7,
    Pb8,
    Pb9,
    PbHash,
}
#[derive(Debug)]
pub enum MobileEmoji {
    Zzz,
    Man,
    Clock,
    Mail,
    MailTo,
    Phone,
    PhoneTo,
    FaxTo,
}
#[derive(Debug)]
pub enum StyleSpecifierStart {
    ColorBlock,
    SizeBlock,
    ColorSwitch,
    SizeSwitch,
}
#[derive(Debug)]
pub enum StyleKind {
    Span,
    Ins,
    Del,
    Em,
    Strong,
}

fn make_line_rules(str: &str) -> impl Iterator<Item = InlineToken> + '_ {
    let regex = regex!(
        r"(?x)
        &(?:
            \#(?P<entity_decimal>[0-9]+)
            |
            \#x(?P<entity_hex>[0-9a-f]+)
            |
            (?P<entity_named>A(?:Elig|acute|circ|grave|lpha|ring|tilde|uml)|Beta|C(?:cedil|hi)|D(?:agger|elta)|E(?:TH|acute|circ|grave|psilon|ta|uml)|Gamma|I(?:acute|circ|grave|ota|uml)|Kappa|Lambda|Mu|N(?:tilde|u)|O(?:Elig|acute|circ|grave|m(?:ega|icron)|slash|tilde|uml)|P(?:hi|i|rime|si)|Rho|S(?:caron|igma)|T(?:HORN|au|heta)|U(?:acute|circ|grave|psilon|uml)|Xi|Y(?:acute|uml)|Zeta|a(?:acute|c(?:irc|ute)|elig|grave|l(?:efsym|pha)|mp|n(?:d|g)|pos|ring|symp|tilde|uml)|b(?:dquo|eta|rvbar|ull)|c(?:ap|cedil|e(?:dil|nt)|hi|irc|lubs|o(?:ng|py)|rarr|u(?:p|rren))|d(?:Arr|a(?:gger|rr)|e(?:g|lta)|i(?:ams|vide))|e(?:acute|circ|grave|m(?:pty|sp)|nsp|psilon|quiv|t(?:a|h)|u(?:ml|ro)|xist)|f(?:nof|orall|ra(?:c(?:1(?:2|4)|34)|sl))|g(?:amma|e|t)|h(?:Arr|arr|e(?:arts|llip))|i(?:acute|circ|excl|grave|mage|n(?:fin|t)|ota|quest|sin|uml)|kappa|l(?:Arr|a(?:mbda|ng|quo|rr)|ceil|dquo|e|floor|o(?:wast|z)|rm|s(?:aquo|quo)|t)|m(?:acr|dash|i(?:cro|ddot|nus)|u)|n(?:abla|bsp|dash|e|i|ot(?:in)?|sub|tilde|u)|o(?:acute|circ|elig|grave|line|m(?:ega|icron)|plus|r(?:d(?:f|m))?|slash|ti(?:lde|mes)|uml)|p(?:ar(?:a|t)|er(?:mil|p)|hi|i(?:v)?|lusmn|ound|r(?:ime|o(?:d|p))|si)|quot|r(?:Arr|a(?:dic|ng|quo|rr)|ceil|dquo|e(?:al|g)|floor|ho|lm|s(?:aquo|quo))|s(?:bquo|caron|dot|ect|hy|i(?:gma(?:f)?|m)|pades|u(?:b(?:e)?|m|p(?:1|2|3|e)?)|zlig)|t(?:au|h(?:e(?:re4|ta(?:sym)?)|insp|orn)|i(?:lde|mes)|rade)|u(?:Arr|a(?:cute|rr)|circ|grave|ml|psi(?:h|lon)|uml)|weierp|xi|y(?:acute|en|uml)|z(?:eta|w(?:j|nj)))
        );
        | (?P<newline> \r)

        # face marks
        | (?P<face_smile0>      \s:\)       )
        | (?P<face_bigsmile0>   \s:D        )
        | (?P<face_huh0>        \s:p        )
        | (?P<face_huh1>        \s:d        )
        | (?P<face_oh0>         \sXD        )
        | (?P<face_oh1>         \sX\(       )
        | (?P<face_wink0>       \s;\)       )
        | (?P<face_sad0>        \s;\(       )
        | (?P<face_sad1>        \s:\(       )

        # face marks (amp)
        | (?P<face_smile1>      &smile;     )
        | (?P<face_bigsmile1>   &bigsmile;  )
        | (?P<face_huh2>        &huh;       )
        | (?P<face_oh2>         &oh;        )
        | (?P<face_wink1>       &wink;      )
        | (?P<face_sad2>        &sad;       )
        | (?P<face_heart0>      &heart;     )
        | (?P<face_worried0>    &worried;   )

        # face marks (japanese)
        | (?P<face_smile2>      \s\(\^\^\)  )
        | (?P<face_bigsmile2>   \s\(\^-\^   )
        | (?P<face_oh3>         \s\(\.\.;   )
        | (?P<face_wink2>       \s\(\^_-\)  )
        | (?P<face_sad3>        \s\(--;     )
        | (?P<face_worried1>    \s\(\^\^;\) )
        | (?P<face_worried2>    \s\(\^\^;   )

        # mobile push buttons
        | (?P<mobile_1>         &pb1;       )
        | (?P<mobile_2>         &pb2;       )
        | (?P<mobile_3>         &pb3;       )
        | (?P<mobile_4>         &pb4;       )
        | (?P<mobile_5>         &pb5;       )
        | (?P<mobile_6>         &pb6;       )
        | (?P<mobile_7>         &pb7;       )
        | (?P<mobile_8>         &pb8;       )
        | (?P<mobile_9>         &pb9;       )
        | (?P<mobile_0>         &pb0;       )
        | (?P<mobile_h>         &pb\#;      )

        # mobile emojis
        | (?P<amp_zzz>          &zzz;       )
        | (?P<amp_man>          &man;       )
        | (?P<amp_clock>        &clock;     )
        | (?P<amp_mail>         &mail;      )
        | (?P<amp_mailto>       &mailto;    )
        | (?P<amp_phone>        &phone;     )
        | (?P<amp_phoneto>      &phoneto;   )
        | (?P<amp_faxto>        &faxto;     )
    "
    );

    let color_blocks = get_color_blocks(str);
    let size_blocks = get_size_blocks(str, &color_blocks);
    let color_switches = get_color_switches(str, &color_blocks, &size_blocks);
    let size_switches = get_size_switches(str, &color_blocks, &size_blocks, &color_switches);
    let (inses, dels) = get_format_ranges(str, regex!("%{2,}"));
    let (ems, strongs) = get_format_ranges(str, regex!("'{2,}"));

    use InlineToken::{StyleEnd, StyleStart};
    use StyleKind::*;
    use StyleSpecifierStart::*;

    // TODO [perf] due to the lifetime constraint, we have to temporarily store this in a vec
    let other_matches = regex
        .captures_iter(str)
        .remove_overlapping(color_blocks.iter().flat_map(|x| [&x.kw, &x.mid, &x.close]))
        .remove_overlapping(size_blocks.iter().flat_map(|x| [&x.kw, &x.mid, &x.close]))
        .remove_overlapping(color_switches.iter().flat_map(|x| [&x.kw, &x.delim]))
        .remove_overlapping(size_switches.iter().flat_map(|x| [&x.kw, &x.delim]))
        .remove_overlapping(inses.iter().flat_map(|x| [&x.start, &x.end]))
        .remove_overlapping(dels.iter().flat_map(|x| [&x.start, &x.end]))
        .remove_overlapping(ems.iter().flat_map(|x| [&x.start, &x.end]))
        .remove_overlapping(strongs.iter().flat_map(|x| [&x.start, &x.end]))
        .collect_vec();

    // lexicographical order of the first element
    let cmp =
        |x: &(Range<usize>, _), y: &(Range<usize>, _)| (x.0.start, x.0.end) < (y.0.start, y.0.end);

    other_matches
        .into_iter()
        .map(|x| (x.start_pos()..x.end_pos(), match_to_token(x)))
        .merge_by(
            color_blocks.into_iter().flat_map(|x| {
                [
                    (x.kw, ColorBlock.into()),
                    (x.mid, StyleStart(Span)),
                    (x.close, StyleEnd(Span)),
                ]
            }),
            cmp,
        )
        .merge_by(
            size_blocks.into_iter().flat_map(|x| {
                [
                    (x.kw, SizeBlock.into()),
                    (x.mid, StyleStart(Span)),
                    (x.close, StyleEnd(Span)),
                ]
            }),
            cmp,
        )
        .merge_by(
            color_switches.into_iter().flat_map(|x| {
                [
                    (x.kw, ColorSwitch.into()),
                    (x.delim, StyleStart(Span)),
                    (x.end..x.end, StyleEnd(Span)),
                ]
            }),
            cmp,
        )
        .merge_by(
            size_switches.into_iter().flat_map(|x| {
                [
                    (x.kw, SizeSwitch.into()),
                    (x.delim, StyleStart(Span)),
                    (x.end..x.end, StyleEnd(Span)),
                ]
            }),
            cmp,
        )
        .merge_by(
            inses
                .into_iter()
                .flat_map(|x| [(x.start, StyleStart(Ins)), (x.end, StyleEnd(Ins))]),
            cmp,
        )
        .merge_by(
            dels.into_iter()
                .flat_map(|x| [(x.start, StyleStart(Del)), (x.end, StyleEnd(Del))]),
            cmp,
        )
        .merge_by(
            ems.into_iter()
                .flat_map(|x| [(x.start, StyleStart(Em)), (x.end, StyleEnd(Em))]),
            cmp,
        )
        .merge_by(
            strongs
                .into_iter()
                .flat_map(|x| [(x.start, StyleStart(Strong)), (x.end, StyleEnd(Strong))]),
            cmp,
        )
        .map(|x| x.1)
}

fn match_to_token(captures: Captures) -> InlineToken {
    use FaceMark::*;
    use InlineToken as IT;
    use MobileEmoji::*;
    use PushButton::*;

    static ENTITY_MAP: Lazy<HashMap<&str, &Entity>> =
        once_cell::sync::Lazy::new(|| ENTITIES.iter().map(|e| (e.entity, e)).collect());

    let as_charcode = |a: Result<u32, _>| match html_charcode(a.unwrap_or(u32::MAX)) {
        Ok(c) => IT::SpecialChar(c.into()),
        Err(HtmlCharcodeError::Replace) => IT::SpecialChar(SpecialChar::ReplacementCharacter),
        Err(HtmlCharcodeError::AsIs) => captures.get(0).unwrap().as_str().to_owned().into(),
    };

    let has_match = |s| captures.name(s).is_some();

    if let Some(entity_decimal) = captures.name("entity_decimal") {
        as_charcode(entity_decimal.as_str().parse())
    } else if let Some(entity_hex) = captures.name("entity_hex") {
        as_charcode(u32::from_str_radix(entity_hex.as_str(), 16))
    } else if let Some(entity_named) = captures.name("entity_named") {
        let entity = *ENTITY_MAP.get(entity_named.as_str()).unwrap();
        IT::SpecialChar(entity.into())
    } else if has_match("newline") {
        IT::NewLine
    } else if any(["face_smile0", "face_smile1", "face_smile2"], has_match) {
        Smile.into()
    } else if any(
        ["face_bigsmile0", "face_bigsmile1", "face_bigsmile2"],
        has_match,
    ) {
        BigSmile.into()
    } else if any(["face_huh0", "face_huh1", "face_huh2"], has_match) {
        Huh.into()
    } else if any(["face_oh0", "face_oh1", "face_oh2", "face_oh3"], has_match) {
        Oh.into()
    } else if any(["face_wink0", "face_wink1", "face_wink2"], has_match) {
        Wink.into()
    } else if any(
        ["face_sad0", "face_sad1", "face_sad2", "face_sad3"],
        has_match,
    ) {
        Sad.into()
    } else if has_match("face_heart0") {
        Heart.into()
    } else if any(
        ["face_worried0", "face_worried1", "face_worried2"],
        has_match,
    ) {
        Worried.into()
    } else {
        match () {
            _ if has_match("mobile_0") => Pb0.into(),
            _ if has_match("mobile_1") => Pb1.into(),
            _ if has_match("mobile_2") => Pb2.into(),
            _ if has_match("mobile_3") => Pb3.into(),
            _ if has_match("mobile_4") => Pb4.into(),
            _ if has_match("mobile_5") => Pb5.into(),
            _ if has_match("mobile_6") => Pb6.into(),
            _ if has_match("mobile_7") => Pb7.into(),
            _ if has_match("mobile_8") => Pb8.into(),
            _ if has_match("mobile_9") => Pb9.into(),
            _ if has_match("mobile_h") => PbHash.into(),
            _ if has_match("amp_zzz") => Zzz.into(),
            _ if has_match("amp_man") => Man.into(),
            _ if has_match("amp_clock") => Clock.into(),
            _ if has_match("amp_mail") => Mail.into(),
            _ if has_match("amp_mailto") => MailTo.into(),
            _ if has_match("amp_phone") => Phone.into(),
            _ if has_match("amp_phoneto") => PhoneTo.into(),
            _ if has_match("amp_faxto") => FaxTo.into(),
            _ => unreachable!("Guarded by regex"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct BraceBlock {
    /// `COLOR(` or `SIZE(`
    kw: Range<usize>,
    /// `){`
    mid: Range<usize>,
    /// `}`
    close: Range<usize>,
}
fn get_brace_blocks(
    str: &str,
    keyword: &str,
    mut middle_iter: Peekable<impl Iterator<Item = Range<usize>>>,
    mut close_iter: Peekable<impl Iterator<Item = Range<usize>>>,
    mut paren_iter: Peekable<impl Iterator<Item = Range<usize>>>,
) -> Vec<BraceBlock> {
    let mut res = vec![];
    let mut last = 0;
    for kw in find_iter_str(str, keyword) {
        if kw.start < last {
            continue;
        }
        dbg!(&kw);
        let mid = unwrap_break!(dbg!(middle_iter.peeking_find(|x| kw.end <= x.start)));
        let next_paren = dbg!(paren_iter
            .peeking_find(|x| kw.end <= x.start)
            .expect("){ is found"));
        if next_paren.start < mid.start {
            continue;
        }
        let mid = middle_iter.next().expect("proven by existence of mid"); // Peeked::pop(mid);
        let close = unwrap_break!(dbg!(close_iter.find(|x| mid.end <= x.start)));
        last = close.end;
        res.push(BraceBlock { kw, mid, close });
    }
    res
}
const PARENS: &[char] = &['(', ')'];
fn get_color_blocks(str: &str) -> Vec<BraceBlock> {
    let middle_iter = find_iter_str(str, "){").peekable();
    let close_iter = find_iter_char(str, '}').peekable();
    let paren_iter = find_iter_char_any(str, PARENS).peekable();
    get_brace_blocks(str, "COLOR(", middle_iter, close_iter, paren_iter)
}
fn get_size_blocks(str: &str, color_blocks: &[BraceBlock]) -> Vec<BraceBlock> {
    let middle_iter = find_iter_str(str, "){")
        .remove_overlapping(color_blocks.iter().map(|x| &x.mid))
        .peekable();
    let close_iter = find_iter_char(str, '}')
        .remove_overlapping(color_blocks.iter().map(|x| &x.close))
        .peekable();
    let paren_iter = find_iter_char_any(str, PARENS)
        .remove_overlapping(color_blocks.iter().flat_map(|x| [&x.kw, &x.mid]))
        .peekable();
    get_brace_blocks(str, "SIZE(", middle_iter, close_iter, paren_iter)
}

struct FormatSwitch {
    /// `COLOR(` or `SIZE(`
    kw: Range<usize>,
    /// `):`
    delim: Range<usize>,
    /// end
    end: usize,
}
fn get_format_switch(
    str: &str,
    keyword: &str,
    mut delim_iter: Peekable<impl Iterator<Item = Range<usize>> + Clone>,
    mut paren_iter: Peekable<impl Iterator<Item = Range<usize>>>,
) -> Vec<FormatSwitch> {
    let n = str.len();
    let keywords = find_iter_str(str, keyword).filter_map(move |kw| {
        dbg!(&kw);
        let delim = dbg!(delim_iter.peeking_find(|d| kw.end <= d.start))?;
        Some((kw, delim.clone()))
    });
    let mut keywords_next = keywords.clone().map(|x| x.0.start).peekable();
    let mut res = vec![];
    let mut last = 0;
    for (kw, delim) in keywords {
        if kw.start < last {
            continue;
        }
        dbg!(&kw);
        dbg!(&delim);
        let &end = keywords_next
            .peeking_find(|&next| delim.end <= next)
            .unwrap_or(&n);
        dbg!(&end);
        let next_paren = paren_iter
            .peeking_find(|x| kw.end <= x.start)
            .expect("): is found");
        if next_paren.start < delim.start {
            continue;
        }
        last = end;
        res.push(FormatSwitch { kw, delim, end });
    }
    res
}
fn get_color_switches(
    str: &str,
    color_blocks: &[BraceBlock],
    size_blocks: &[BraceBlock],
) -> Vec<FormatSwitch> {
    let delim_iter = find_iter_str(str, "):").peekable();
    let paren_iter = find_iter_char_any(str, PARENS)
        .remove_overlapping(color_blocks.iter().flat_map(|x| [&x.kw, &x.mid]))
        .remove_overlapping(size_blocks.iter().flat_map(|x| [&x.kw, &x.mid]))
        .peekable();
    get_format_switch(str, "COLOR(", delim_iter, paren_iter)
}
fn get_size_switches(
    str: &str,
    color_blocks: &[BraceBlock],
    size_blocks: &[BraceBlock],
    color_switches: &[FormatSwitch],
) -> Vec<FormatSwitch> {
    let delim_iter = find_iter_str(str, "):")
        .remove_overlapping(color_switches.iter().map(|x| &x.delim))
        .peekable();
    let paren_iter = find_iter_char_any(str, PARENS)
        .remove_overlapping(color_blocks.iter().flat_map(|x| [&x.kw, &x.mid]))
        .remove_overlapping(size_blocks.iter().flat_map(|x| [&x.kw, &x.mid]))
        .remove_overlapping(color_switches.iter().flat_map(|x| [&x.kw, &x.delim]))
        .peekable();
    get_format_switch(str, "SIZE(", delim_iter, paren_iter)
}

struct FormatRange {
    start: Range<usize>,
    end: Range<usize>,
}
fn get_format_ranges(str: &str, regex: &Regex) -> (Vec<FormatRange>, Vec<FormatRange>) {
    let mut positions = regex
        .find_iter(str)
        .map(|x| x.start()..x.end())
        .collect_vec();
    let mut res = (vec![], vec![]);
    for (res, len) in zip([&mut res.0, &mut res.1], [3, 2]) {
        dbg!(&positions, len);
        let find = |slice: &[Range<usize>]| (0..slice.len()).find(|&i| slice[i].len() >= len);
        let mut s = find(&positions);
        while let Some(i) = s {
            let (bef, aft) = positions.split_at_mut(i + 1);
            let bef = &mut bef[i];
            let j = unwrap_break!(find(aft));
            let aft = &mut aft[j];
            res.push(FormatRange {
                start: bef.end - len..bef.end,
                end: aft.start..aft.start + len,
            });
            bef.end -= len;
            aft.start += len;
            s = find(&positions[i + j + 1..]).map(|k| i + j + 1 + k);
        }
    }
    res
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum HtmlCharcodeError {
    Replace,
    AsIs,
}
fn html_charcode(charcode: u32) -> Result<char, HtmlCharcodeError> {
    use HtmlCharcodeError::*;
    match charcode {
        0 => Err(Replace),                                   // null
        x if x > 0x10FFFF => Err(Replace),                   // replace
        x if (0xD800..=0xDFFF).contains(&x) => Err(Replace), // Surrogate
        x if (0xFDD0..=0xFDEF).contains(&x) => Err(Replace), // Non-character
        0xFFFE | 0xFFFF | 0x1FFFE | 0x1FFFF | 0x2FFFE | 0x2FFFF | 0x3FFFE | 0x3FFFF | 0x4FFFE
        | 0x4FFFF | 0x5FFFE | 0x5FFFF | 0x6FFFE | 0x6FFFF | 0x7FFFE | 0x7FFFF | 0x8FFFE
        | 0x8FFFF | 0x9FFFE | 0x9FFFF | 0xAFFFE | 0xAFFFF | 0xBFFFE | 0xBFFFF | 0xCFFFE
        | 0xCFFFF | 0xDFFFE | 0xDFFFF | 0xEFFFE | 0xEFFFF | 0xFFFFE | 0xFFFFF | 0x10FFFE
        | 0x10FFFF => Err(Replace), // Non-character
        0x0D => Err(AsIs),
        x if (0x00..=0x1F).contains(&x) || (0x7F..0x9F).contains(&x) => {
            Ok(char::from_u32(match x {
                // ASCII control sequence
                0x09 | 0x0A | 0x0C | 0x0D | 0x20 => x, // ASCII whitespace
                0x80 => 0x20AC,
                0x82 => 0x201A,
                0x83 => 0x0192,
                0x84 => 0x201E,
                0x85 => 0x2026,
                0x86 => 0x2020,
                0x87 => 0x2021,
                0x88 => 0x02C6,
                0x89 => 0x2030,
                0x8A => 0x0160,
                0x8B => 0x2039,
                0x8C => 0x0152,
                0x8E => 0x017D,
                0x91 => 0x2018,
                0x92 => 0x2019,
                0x93 => 0x201C,
                0x94 => 0x201D,
                0x95 => 0x2022,
                0x96 => 0x2013,
                0x97 => 0x2014,
                0x98 => 0x02DC,
                0x99 => 0x2122,
                0x9A => 0x0161,
                0x9B => 0x203A,
                0x9C => 0x0153,
                0x9E => 0x017E,
                0x9F => 0x0178,
                _ => return Err(AsIs),
            })
            .unwrap())
        }
        x => Ok(char::from_u32(x).unwrap()),
    }
}

#[derive(Debug)]
pub struct PageLink {
    page: Option<String>,
    contents: Vec<InlineElement>,
    anchor: Option<String>,
    is_auto_link: bool,
}
fn make_page_link(
    page: impl Into<Option<String>>,
    contents: Vec<InlineElement>,
    anchor: Option<String>,
    is_auto_link: bool,
) -> PageLink {
    PageLink {
        page: page.into(),
        contents,
        anchor,
        is_auto_link,
    }
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

fn find_iter_str<'a>(
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

fn find_iter_char(haystack: &str, needle: char) -> impl Iterator<Item = Range<usize>> + '_ + Clone {
    std::iter::successors(Some(0..0), move |r| {
        let i = r.end;
        haystack[i..]
            .find(needle)
            .map(|s| i + s..i + s + needle.len_utf8())
    })
    .skip(1)
}

fn find_iter_char_any<'a>(
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
mod test {
    use crate::pukiwiki_parser::parser::{
        as_numeric, get_color_switches, get_size_blocks, get_size_switches, is_url,
    };
    use crate::regex;

    use super::{get_color_blocks, get_format_ranges};

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

    #[test]
    fn test_is_url() {
        assert!(is_url("https://example.com/example.png"));
        assert!(!is_url("https://example.com/example.png "));
        assert!(!is_url("   https://example.com/example.png"));
        assert!(!is_url("this_is_not_url"));
    }

    macro_rules! assert_blocks {
        ($lhs: expr, [$($rhs: expr),* $(,)?]) => {{
            use ::itertools::Itertools;
            assert_eq!(
                $lhs.into_iter().map(|x| (x.kw, x.mid, x.close)).collect_vec(),
                vec![$($rhs),*]
            );
        }};
    }

    macro_rules! assert_switches {
        ($lhs: expr, [$($rhs: expr),* $(,)?]) => {{
            use ::itertools::Itertools;
            assert_eq!(
                $lhs.into_iter().map(|x| (x.kw, x.delim, x.end)).collect_vec(),
                vec![$($rhs),*]
            );
        }};
    }

    macro_rules! assert_ranges {
        ($lhs: expr, [$($rhs: expr),* $(,)?]) => {{
            use ::itertools::Itertools;
            assert_eq!(
                $lhs.into_iter().map(|x| (x.start, x.end)).collect_vec(),
                vec![$($rhs),*]
            );
        }};
    }

    #[test]
    fn test_color_blocks() {
        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(blue){test}";
        let res = get_color_blocks(str);
        assert_blocks!(res, [(0..6, 10..12, 16..17)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "    COLOR(blue){    { COLOR(red){   {  }}}}";
        let res = get_color_blocks(str);
        assert_blocks!(res, [(4..10, 14..16, 39..40)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "    COLOR( COLOR(green){ test } ){ another test }";
        let res = get_color_blocks(str);
        assert_blocks!(res, [(11..17, 22..24, 30..31)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(red){text}COLOR(blue){another}";
        let res = get_color_blocks(str);
        assert_blocks!(res, [(0..6, 9..11, 15..16), (16..22, 26..28, 35..36)]);
    }

    #[test]
    fn test_size_blocks() {
        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "  SIZE(13){large text}  ";
        let colors = get_color_blocks(str);
        let sizes = get_size_blocks(str, &colors);
        assert_blocks!(colors, []);
        assert_blocks!(sizes, [(2..7, 9..11, 21..22)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "SIZE(13){COLOR(red){text}}";
        let colors = get_color_blocks(str);
        let sizes = get_size_blocks(str, &colors);
        assert_blocks!(colors, [(9..15, 18..20, 24..25)]);
        assert_blocks!(sizes, [(0..5, 7..9, 25..26)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(red){SIZE(13){text}}";
        let colors = get_color_blocks(str);
        let sizes = get_size_blocks(str, &colors);
        assert_blocks!(colors, [(0..6, 9..11, 24..25)]);
        assert_blocks!(sizes, [(11..16, 18..20, 25..26)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "SIZE(COLOR(red){text}){another}";
        let colors = get_color_blocks(str);
        let sizes = get_size_blocks(str, &colors);
        assert_blocks!(colors, [(5..11, 14..16, 20..21)]);
        assert_blocks!(sizes, [(0..5, 21..23, 30..31)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(SIZE(18){text}){another}";
        let colors = get_color_blocks(str);
        let sizes = get_size_blocks(str, &colors);
        assert_blocks!(colors, []);
        assert_blocks!(sizes, [(6..11, 13..15, 19..20)]);
    }

    #[test]
    fn test_color_switches() {
        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "no color COLOR(red):this is a text COLOR(blue):this is another test";
        let color_blocks = get_color_blocks(str);
        let size_blocks = get_size_blocks(str, &color_blocks);
        let color_switches = get_color_switches(str, &color_blocks, &size_blocks);
        assert_blocks!(color_blocks, []);
        assert_blocks!(size_blocks, []);
        assert_switches!(color_switches, [(9..15, 18..20, 35), (35..41, 45..47, 67)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(  COLOR(red):test COLOR( COLOR(green): another  COLOR( ():";
        let color_blocks = get_color_blocks(str);
        let size_blocks = get_size_blocks(str, &color_blocks);
        let color_switches = get_color_switches(str, &color_blocks, &size_blocks);
        assert_blocks!(color_blocks, []);
        assert_blocks!(size_blocks, []);
        assert_switches!(color_switches, [(8..14, 17..19, 24), (31..37, 42..44, 54)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(SIZE(COLOR(red){text}){another}):test";
        let color_blocks = get_color_blocks(str);
        let size_blocks = get_size_blocks(str, &color_blocks);
        let color_switches = get_color_switches(str, &color_blocks, &size_blocks);
        assert_blocks!(color_blocks, [(11..17, 20..22, 26..27)]);
        assert_blocks!(size_blocks, [(6..11, 27..29, 36..37)]);
        assert_switches!(color_switches, [(0..6, 37..39, 43)]);
    }

    #[test]
    fn test_size_switches() {
        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "SIZE(COLOR(SIZE(COLOR(red){p}){q}):r):s COLOR(blue):t SIZE(12):u";
        let color_blocks = get_color_blocks(str);
        let size_blocks = get_size_blocks(str, &color_blocks);
        let color_switches = get_color_switches(str, &color_blocks, &size_blocks);
        let size_switches = get_size_switches(str, &color_blocks, &size_blocks, &color_switches);
        assert_blocks!(color_blocks, [(16..22, 25..27, 28..29)]);
        assert_blocks!(size_blocks, [(11..16, 29..31, 32..33)]);
        assert_switches!(color_switches, [(5..11, 33..35, 40), (40..46, 50..52, 64)]);
        assert_switches!(size_switches, [(0..5, 36..38, 54), (54..59, 61..63, 64)]);
    }

    #[test]
    fn test_get_format_ranges() {
        let regex = regex!(r"%{2,}");

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "  %%% %%%    %% %%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(2..5, 6..9)]);
        assert_ranges!(dva, [(13..15, 16..18)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "  %%%%%   %%%%%%%  %%%%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(4..7, 10..13), (14..17, 19..22)]);
        assert_ranges!(dva, []);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "  %%%%%   %%%%%%%  %%%%%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(4..7, 10..13), (14..17, 19..22)]);
        assert_ranges!(dva, [(2..4, 22..24)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "  %%%%%   %%%%%%%%  %%%%%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(4..7, 10..13), (15..18, 20..23)]);
        assert_ranges!(dva, [(2..4, 13..15)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "%% %%% %% %%%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(3..6, 10..13)]);
        assert_ranges!(dva, [(0..2, 7..9)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "%% %%% %%% %%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(3..6, 7..10)]);
        assert_ranges!(dva, [(0..2, 11..13)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "%%% %%%% %%% %%%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(0..3, 4..7), (9..12, 13..16)]);
        assert_ranges!(dva, []);
    }
}
