extern crate reqwest;

use if_chain::if_chain;

use encoding_rs::DecoderResult;
use scraper::Html;
use std::error;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use taiko_wiki_data_analysis::pukiwiki_reparser::structs::{
    table, BodyElement, Document, InlineElement, InlineElements,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn error::Error>> {
    let reqwest_client: reqwest::Client = reqwest::Client::new();
    let mut decoder = encoding_rs::EUC_JP.new_decoder();

    let wiki_dir = Path::new("dump-data").join("wiki");
    let path =
        wiki_dir.join("C2C0B8DDA4CEC3A3BFCD20BFB7E3FEC2CEA4CEBCFDCFBFB6CA2FA5B0A5EAA1BCA5F3.txt");
    let song_list_page = get_wiki(&mut decoder, &reqwest_client, path).await?;
    let parse_result = Document::parse(&song_list_page);

    let table = (parse_result.0)
        .0
        .iter()
        .filter_map(|x| match x {
            BodyElement::Table(t) => Some(t),
            _ => None,
        })
        .max_by_key(|t| t.body.len())
        .unwrap();
    for (genre, song) in table_to_songs(table) {
        println!("{}\t{}", genre, song);
    }

    Ok(())
}

type SongEntry = (String, String);
// type SongEntry<'a> = (&'a String, &'a String);

fn table_to_songs(table: &table::Table) -> impl Iterator<Item = SongEntry> + '_ {
    table
        .body
        .iter()
        .scan(Option::<String>::None, |bef, row| match row.0.len() {
            1 => {
                *bef = Some(row_to_genre_name(row));
                Some(None)
            }
            10 => Some(Some(row_to_song_entry(bef.to_owned().unwrap(), row))),
            _ => panic!(),
        })
        .flatten()
}

fn row_to_genre_name(row: &table::Row) -> String {
    if_chain! {
        if let [ table::Cell { contents: InlineElements(ref elements), .. } ] = row.0[..];
        if let [ InlineElement::Strong(InlineElements(ref elements)), InlineElement::Anchor {..} ] = elements[..];
        if let [ InlineElement::Text(ref text) ] = elements[..];
        then {
            text.to_string()
        } else {
            panic!();
        }
    }
}

fn row_to_song_entry(genre_name: String, row: &table::Row) -> SongEntry {
    let song_name = match row.0[3].contents.0[..] {
        [InlineElement::Strong(InlineElements(ref text))]
        | [InlineElement::Strong(InlineElements(ref text)), _, _] => {
            if let [InlineElement::Text(text)] = &text[..] {
                text
            } else {
                panic!()
            }
        }
        ref others => {
            panic!("{:#?}", others);
        }
    };

    (genre_name, song_name.to_owned())
}

async fn get_wiki<P>(
    decoder: &mut encoding_rs::Decoder,
    reqwest_client: &reqwest::Client,
    path: P,
) -> Result<Html, Box<dyn error::Error>>
where
    P: AsRef<Path>,
{
    let mut file = File::open(path)?;
    let mut buf = Vec::new();
    let _ = file.read_to_end(&mut buf)?;
    let mut str = String::with_capacity(
        decoder
            .max_utf8_buffer_length_without_replacement(buf.len())
            .unwrap(),
    );
    if let result @ DecoderResult::Malformed(_, _) | result @ DecoderResult::OutputFull = decoder
        .decode_to_string_without_replacement(&buf, &mut str, true)
        .0
    {
        panic!(";{:?}", result)
    }
    let song_list_page: reqwest::Response = reqwest_client
        .post("http://localhost:8765/perform_conversion.php")
        .body(str)
        .send()
        .await?;
    Ok(Html::parse_document(&song_list_page.text().await?))
}
