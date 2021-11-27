use clap::Parser;
use encoding_rs::EUC_JP;

#[derive(Parser)]
struct Opts {
    arg: String,
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();
    let hex = hex::decode(opts.arg)?;
    let (res, encoding_used, had_errors) = EUC_JP.decode(&hex);
    dbg!(encoding_used, had_errors);
    println!("{}", res);

    Ok(())
}
