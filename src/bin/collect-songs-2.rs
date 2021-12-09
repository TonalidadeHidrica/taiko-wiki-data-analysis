use std::path::PathBuf;

use anyhow::anyhow;
use clap::Parser;
use itertools::Itertools;
use taiko_wiki_data_analysis::pukiwiki_parser::{
    block::{Element, TableCell, TableRowKind},
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
    let rows = largest_table(&elements)
        .ok_or(anyhow!("There is no table in this page"))?
        .iter()
        .filter_map(|x| match x {
            Element::Table(table) if table.kind() != TableRowKind::Formatter => Some(table),
            _ => None,
        });
    for row in rows {
        match &row.cells()[..] {
            [rem @ .., TableCell::Content(value, style)]
                if rem.iter().all(|x| matches!(x, TableCell::MergeRight)) =>
            {
                let color = style.background_color();
                println!("{:?} {:?}", color, value);
            }
            _ => {}
        }
    }
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
