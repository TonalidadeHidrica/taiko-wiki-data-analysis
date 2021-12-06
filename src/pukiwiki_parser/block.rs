use std::borrow::Cow;

use either::*;
use itertools::Itertools;
use regex::Regex;
use getset::{Getters, CopyGetters};

use crate::{
    either_ext::{into_common_2, EitherExt},
    my_itertools::MyItertools,
    regex,
};

use super::{
    config::Config,
    inline::{make_link, InlineElement},
    php::as_numeric,
    str_ext::{strip_prefix_cow, strip_prefix_n, trim_cow},
};

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
#[derive(Debug, Getters)]
#[getset(get="pub")]
pub struct Inline<'a> {
    // src: Cow<'a, str>,
    elements: Vec<InlineElement>,
    /// Maybe we are going to use this lifetime afterwards
    #[getset(skip)]
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
    #[allow(unused)]
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

#[derive(Debug, Getters, CopyGetters)]
pub struct Heading<'a> {
    #[getset(get_copy="pub")]
    level: HeadingLevel,
    #[getset(get_copy="pub")]
    tag: Option<&'a str>,
    #[getset(get="pub")]
    text: FactoryInlineRet<'a>,
}
#[derive(Clone, Copy, Debug)]
pub enum HeadingLevel {
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

#[derive(Debug, Getters, CopyGetters)]
pub struct List<'a> {
    #[getset(get_copy="pub")]
    kind: ListKind,
    #[getset(get_copy="pub")]
    level: usize,
    #[getset(get="pub")]
    text: FactoryInlineRet<'a>,
}
#[derive(Clone, Copy, Debug)]
pub enum ListKind {
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

#[derive(Debug, Getters, CopyGetters)]
pub struct DList<'a> {
    #[getset(get_copy="pub")]
    level: usize,
    #[getset(get="pub")]
    word: FactoryInlineRet<'a>,
    #[getset(get="pub")]
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

#[derive(Debug, Getters, CopyGetters)]
pub struct BQuote<'a> {
    #[getset(get_copy="pub")]
    level: usize,
    #[getset(get="pub")]
    kind: BQuoteKind,
    #[getset(get="pub")]
    text: Option<FactoryInlineRet<'a>>,
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum BQuoteKind {
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
pub enum TableCell<'a> {
    MergeRight,
    MergeAbove,
    Content(TableContent<'a>),
}
#[derive(Debug, Getters, CopyGetters)]
pub struct TableContent<'a> {
    #[getset(get_copy="pub")]
    is_header: bool,
    #[getset(get="pub")]
    child: TableContentChild<'a>,
}
#[derive(Debug)]
pub enum TableContentChild<'a> {
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

#[derive(Debug, Getters)]
#[getset(get="pub")]
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

#[derive(Debug, Getters, CopyGetters)]
pub struct YTableCell<'a> {
    #[getset(get_copy="pub")]
    align: Align,
    #[getset(get="pub")]
    content: YTableContent<'a>,
}
#[derive(Debug)]
pub enum YTableContent<'a> {
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

#[derive(Debug, Getters)]
#[getset(get="pub")]
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
#[derive(Debug, CopyGetters)]
#[getset(get_copy="pub")]
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

#[derive(Debug, Getters, CopyGetters)]
pub struct Div<'a> {
    #[getset(get_copy="pub")]
    plugin_name: &'a str,
    #[getset(get_copy="pub")]
    args: Option<&'a str>,
    #[getset(get="pub")]
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
