use either::*;
use itertools::Itertools;
use regex::Regex;

use crate::{my_itertools::MyItertools, regex};

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
    fn taiko_wiki() -> Self {
        Self {
            disable_multiline_plugin: false,
            ..Default::default()
        }
    }
}

type FactoryInlineRet = Either<Paragraph, Inline>;
fn factory_inline(text: &str) -> FactoryInlineRet {
    if let Some(text) = text.strip_prefix('~') {
        Left(Paragraph::new(text, ()))
    } else {
        Right(Inline::new(text))
    }
}

fn factory_dlist(text: &str) -> Either<DList, FactoryInlineRet> {
    if let Some((a, b)) = text.split_once('|') {
        Left(DList::new(a, b))
    } else {
        Right(factory_inline(text))
    }
}

fn factory_table<'a>(text: &'a str, config: &Config) -> Either<Table<'a>, FactoryInlineRet> {
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
        Right(factory_inline(text))
    }
}

/// panics if text is empty
fn factory_ytable(text: &str) -> Either<YTable, FactoryInlineRet> {
    if text == "," {
        Right(factory_inline(text))
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
    // TODO: send remaining_lines to paragraph too
    Right(Paragraph::new(text, ()))
}
fn exist_plugin_convert(plugin_name: &str) -> bool {
    true
}

// Inline elements
#[derive(Debug)]
struct Inline {}
impl Inline {
    fn new(text: &str) -> Self {
        if text.starts_with('\n') {
            // $text
        } else {
            make_link(text);
        }
        // TODO: trim
        Self {}
    }
}

// Paragraph: blank-line-separated sentences
#[derive(Debug)]
struct Paragraph {
    param: (),
    elements: Vec<Inline>,
}
impl Paragraph {
    fn new(text: &str, param: ()) -> Self {
        let elements = if text.is_empty() {
            vec![]
        } else {
            let text = text.strip_prefix('~').unwrap_or(text);
            match factory_inline(text) {
                Left(paragraph) => todo!("Insert paragraph: {:?}", paragraph),
                Right(inline) => vec![inline],
            }
        };
        Self { param, elements }
    }
}

#[derive(Debug)]
struct Heading {
    level: HeadingLevel,
}
#[derive(Debug)]
enum HeadingLevel {
    H2,
    H3,
    H4,
}
impl Heading {
    /// # panics
    /// If text does not start with `*`
    fn new(text: &str) -> Self {
        let level = match text.chars().take_while(|&c| c == '*').take(3).count() {
            0 => panic!("Should call with a text starting with '*'"),
            1 => HeadingLevel::H2,
            2 => HeadingLevel::H3,
            3 => HeadingLevel::H4,
            _ => unreachable!("take(3)"),
        };
        // insert to self
        insert(factory_inline(text));
        Self { level }
    }
}

#[derive(Debug)]
struct HRule;

#[derive(Debug)]
struct ListContainer {}
impl ListContainer {
    fn new(_text: &str) -> Self {
        Self {}
    }
}

#[derive(Debug)]
struct ListElement {}
impl ListElement {
    fn new(_text: &str) -> Self {
        Self {}
    }
}

#[derive(Debug)]
struct UList {}
impl UList {
    fn new(_text: &str) -> Self {
        Self {}
    }
}

#[derive(Debug)]
struct OList {}
impl OList {
    fn new(_text: &str) -> Self {
        Self {}
    }
}

#[derive(Debug)]
struct DList {}
impl DList {
    fn new(_text: &str, _: &str) -> Self {
        Self {}
    }
}

#[derive(Debug)]
struct BQuote {}
impl BQuote {
    fn new(_text: &str) -> Self {
        Self {}
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
    Paragraph(Paragraph),
    Inline(Inline),
    Div(Div<'a>),
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
                    Right(mut para) => TableContentChild::Inline(para.elements.swap_remove(0)),
                    // TODO: does children[0] always exist?
                    // TODO: is children always inline?
                }
            } else {
                match factory_inline(text) {
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
        r"[+-]?([0-9]*\.[0-9]+|[0-9]+\.[0-9]*|([0-9]+|[0-9]*\.[0-9]+|[0-9]+\.[0-9]*)[eE][+-][0-9]+)"
    )
    .is_match(val);
    (is_int || is_float).then(|| val.parse().ok()).flatten()
}

#[derive(Debug)]
struct Table<'a> {
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
struct YTableCell {
    align: Align,
    content: YTableContent,
}
#[derive(Debug)]
enum YTableContent {
    MergeRight,
    Content(()),
}
impl YTableCell {
    fn new(text: &str) -> Self {
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
            YTableContent::Content(make_link(text))
        };
        Self { align, content }
    }
}

#[derive(Debug)]
struct YTable {
    cells: Vec<YTableCell>,
}
impl YTable {
    fn new<'a>(elements: impl IntoIterator<Item = &'a str>) -> Self {
        let cells = elements
            .into_iter()
            .map(|text| YTableCell::new(text))
            .collect();
        Self { cells }
    }
}

// ' 'Space-beginning sentence
#[derive(Debug)]
struct Pre<'a> {
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
struct Div<'a> {
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
enum Align {
    Left,
    Center,
    Right,
}

const NEWLINES: &[char] = &['\r', '\n'];

pub fn parse(config: &Config, lines: String) {
    let mut lines = lines.split('\n');
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
        let line = if let Some((align, line)) = align_candidates
            .iter()
            .find_map(|(pat, ret)| line.strip_prefix(pat).map(|rem| (ret, rem)))
        {
            add_last(align);
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
            reset_last();
            continue;
        }

        // Horizontal Rule
        if line.starts_with("----") {
            insert(HRule);
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
                // Last ~ is ignored because in original source it is converted to \r
                // which is indistinguishable from terminal newline (maybe)
                add_last(factory_div(
                    line,
                    remaining_lines,
                    config.disable_multiline_plugin,
                ));
                // That's why we may skip remaining process in this loop
                continue;
            }
        }

        // Heading
        if line.starts_with('*') {
            insert(Heading::new(line));
            continue;
        }

        // Pre
        if line.starts_with(&[' ', '\t'][..]) {
            // $this->last = & $this->last->add(new Pre($this, $line));
            add_last(Pre::new(line, config.preformat_ltrim));
            continue;
        }

        // Line Break
        if let Some(line) = line.strip_suffix('~') {
            // $line = substr($line, 0, -1) . "\r";
        }

        // Other Character
        match line.chars().next().expect("line is non-empty") {
            '-' => add_last(UList::new(line)),
            '+' => add_last(OList::new(line)),
            '>' => add_last(BQuote::new(line)),
            '<' => add_last(BQuote::new(line)),
            ':' => add_last(factory_dlist(line)),
            '|' => add_last(factory_table(line, config)),
            ',' => add_last(factory_ytable(line)),
            '#' => add_last(factory_div(line, [], config.disable_multiline_plugin)),
            _ => add_last(factory_inline(line)),
        };
    }
}

fn get_anchor<'t>(text: &'t str, _level: &str) -> (&'t str, (), ()) {
    // TODO: make_heading($text, FALSE)
    (text, (), ())
}

fn make_link(text: &str) {}

fn add_last(element: impl std::fmt::Debug) {
    println!("add last: {:?}", element);
}
fn reset_last() {
    println!("reset last");
}
fn insert(element: impl std::fmt::Debug) {
    println!("insert: {:?}", element);
}
