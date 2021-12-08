use std::path::PathBuf;

use clap::Parser;
use itertools::Itertools;
use taiko_wiki_data_analysis::pukiwiki_parser::{
    block::{Element, TableRowKind},
    reader::{ReaderConfig, WikiReader},
    ParserConfig,
};

#[derive(Parser)]
struct Opts {
    path: PathBuf,
    page_name: String,
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();
    let mut reader = WikiReader::new(ReaderConfig {
        root_dir: opts.path,
        parser_config: ParserConfig::taiko_wiki(),
    });
    let elements = reader.read(&opts.page_name)?;
    let table = largest_table(&elements);
    println!("{:?}", table);
    Ok(())
}

fn largest_table<'a, 'b: 'a>(elements: &'a [Element<'b>]) -> Option<&'a [Element<'b>]> {
    enum Kind<T> {
        Yes(T),
        Maybe,
        No,
    }
    use Kind::*;
    elements
        .iter()
        .enumerate()
        .map(|x| {
            (
                match &x.1 {
                    Element::Table(t) => Yes(t.cells().len()),
                    Element::NewLine => Maybe,
                    _ => No,
                },
                x.0,
                x.1,
            )
        })
        .dedup_by(|x, y| match (&x.0, &y.0) {
            (Yes(x), Yes(y)) => x == y,
            (Yes(_), Maybe) => true,
            (Yes(_), No) => false,
            (Maybe, Maybe) => true,
            (Maybe, _) => false,
            (No, Yes(_)) => false,
            (No, Maybe | No) => true,
        })
        .map(|x| x.1)
        .chain([elements.len()])
        .tuple_windows()
        .map(|(i, j)| &elements[i..j])
        .max_by_key(|x| {
            x.iter()
                .filter(|x| matches!(x, Element::Table(t) if t.kind() != TableRowKind::Formatter))
                .count()
        })
}
