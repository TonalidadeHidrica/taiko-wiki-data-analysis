use std::iter::{self, once};

use itertools::Itertools;
use regex::Regex;

use crate::{my_itertools::MyItertools, regex};

pub struct Config {
    disbale_multiline_plugin: bool,
}

#[derive(Debug)]
enum Align {
    Left,
    Center,
    Right,
}

struct HRule;

const NEWLINES: &[char] = &['\r', '\n'];

pub fn parse(config: &Config, lines: String) {
    let mut lines = lines.split('\n');
    while let Some(line) = lines.next() {
        let line = &line;

        // Escape comments
        if line.starts_with("//") {
            continue;
        }

        let line = if let Some((align, line)) = None
            .or_else(|| line.strip_prefix("LEFT:").map(|x| (Align::Left, x)))
            .or_else(|| line.strip_prefix("CENTER:").map(|x| (Align::Center, x)))
            .or_else(|| line.strip_prefix("RIGHT:").map(|x| (Align::Right, x)))
        {
            dbg!(align);
            // $this->last = & $this->last->add(new Align(strtolower($matches[1])));
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
            // $this->last = & $this;
            continue;
        }

        // Horizontal Rule
        if line.starts_with("----") {
            // $this->insert(new HRule($this, $line));
            continue;
        }

        // Multiline-enabled block plugin
        // let remaining_lines = if !config.disbale_multiline_plugin {
        //     if let Some(res) = regex!(r"^#[^{]+(\{\{+)\s*$").captures(line) {
        //         let regex = Regex::new(&format!(r"\}}{{{}}}", res[1].len())).unwrap();
        //         Some(
        //             lines
        //                 .by_ref()
        //                 .map(|line| line.trim_end_matches(NEWLINES))
        //                 .take_until(move |line| regex.is_match(line)),
        //         )
        //     } else {
        //         None
        //     }
        // } else {
        //     None
        // };
        // let lines = once(line)
        //     .chain(remaining_lines.into_iter().flatten())
        //     .flat_map(|x| x.chars());
        // let line = Itertools::intersperse(lines, '\r').peekable();

        // Heading
        if line.starts_with('*') {
            // $this->insert(new Heading($this, $line));
            continue;
        }

        // Pre
        if line.starts_with(&[' ', '\t'][..]) {
            // $this->last = & $this->last->add(new Pre($this, $line));
            continue;
        }

        // Line Break
        if let Some(line) = line.strip_suffix('~') {
            // $line = substr($line, 0, -1) . "\r";
        }

        // Other Character
        match line.chars().next().expect("line is non-empty") {
            '-' => "UList",
            '+' => "OList",
            '>' => "BQuote",
            '<' => "BQuote",
            ':' => "DList",
            '|' => "Table",
            ',' => "YTable",
            '#' => "Div",
            _ => "Inline",
        };
    }
}
