use std::{
    io::{BufReader, Read},
    path::PathBuf,
};

use clap::Parser;
use encoding_rs::EUC_JP;
use fs_err::File;
use taiko_wiki_data_analysis::pukiwiki_parser::parser::{parse, Config};

#[derive(Parser)]
struct Opts {
    path: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();

    let mut file = BufReader::new(File::open(&opts.path)?);
    let mut buf = Vec::new();
    let _ = file.read_to_end(&mut buf)?;
    let (str, encoding, replace) = EUC_JP.decode(&buf);
    if encoding != EUC_JP || replace {
        panic!("Malformed input in {:?}", opts.path);
    }
    let parsed = parse(&Config::taiko_wiki(), &str);
    println!("{:#?}", parsed);
    Ok(())
}
