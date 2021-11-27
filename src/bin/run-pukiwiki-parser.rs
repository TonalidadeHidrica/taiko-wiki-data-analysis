use std::{
    io::{BufReader, Read},
    path::PathBuf,
};

use clap::Parser;
use encoding_rs::EUC_JP;
use fs_err::File;
use glob::glob;
use indicatif::ProgressIterator;
use taiko_wiki_data_analysis::pukiwiki_parser::parser::{parse, Config};

#[derive(Parser)]
struct Opts {
    path: PathBuf,
    #[clap(short = 'r', long = "recursive")]
    recursive: bool,
    #[clap(short = 'q', long = "quiet")]
    quiet: bool,
}

fn run(file: impl Into<PathBuf>, quiet: bool) -> anyhow::Result<()> {
    let mut file = BufReader::new(File::open(file)?);
    let mut buf = Vec::new();
    let _ = file.read_to_end(&mut buf)?;
    let (str, encoding, replace) = EUC_JP.decode(&buf);
    if encoding != EUC_JP || replace {
        eprintln!("Malformed input in {:?}", file);
        return Ok(());   // Actually I want to return error, but who cares
    }
    let parsed = parse(&Config::taiko_wiki(), &str);
    if !quiet {
        println!("{:#?}", parsed);
    }
    Ok(())
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();
    if opts.recursive {
        let files = glob(&(opts.path.as_os_str().to_string_lossy() + "/*.txt"))?;
        for file in files.progress_count(17000) {
            run(file?, opts.quiet)?;
        }
    } else {
        run(opts.path, opts.quiet)?;
    }

    Ok(())
}
