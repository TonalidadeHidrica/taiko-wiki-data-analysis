extern crate reqwest;

use encoding_rs::DecoderResult;
use scraper::Html;
use std::error;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use taiko_wiki_data_analysis::pukiwiki_reparser::structs::{BodyElement, Document};

#[tokio::main]
async fn main() -> Result<(), Box<dyn error::Error>> {
    let reqwest_client: reqwest::Client = reqwest::Client::new();
    let mut decoder = encoding_rs::EUC_JP.new_decoder();

    let wiki_dir = Path::new("dump-data").join("wiki");
    let path =
        wiki_dir.join("C2C0B8DDA4CEC3A3BFCD20BFB7E3FEC2CEA4CEBCFDCFBFB6CA2FA5B0A5EAA1BCA5F3.txt");
    let document = get_wiki(&mut decoder, &reqwest_client, path).await?;

    let table = (document.0)
        .0
        .iter()
        .filter_map(|x| match x {
            BodyElement::Table(t) => Some(t),
            _ => None,
        })
        .max_by_key(|t| t.body.len())
        .unwrap();

    for song in song_list_page::table_to_songs(table) {
        dbg!(song);
    }

    Ok(())
}

mod song_list_page {
    use if_chain::if_chain;
    use lazy_static::lazy_static;

    use itertools::Itertools;
    use num_derive::FromPrimitive;
    use regex::Regex;
    use std::collections::HashMap;
    use std::fmt::Debug;
    use taiko_wiki_data_analysis::pukiwiki_reparser::structs::{
        table, InlineElement as IE, InlineElements, LinkType,
    };

    #[derive(Debug, Clone)]
    struct Genre {
        title: String,
    }

    #[derive(Debug)]
    pub(crate) struct Song {
        genre: Genre,
        title: String,
        subtitle: Option<String>,
        scores: HashMap<Level, Score>,
    }

    #[derive(Debug)]
    struct Score {
        page_title: String,
        stars: u8,
        has_branches: bool,
        has_papamama_support: bool,
    }

    #[derive(Debug, PartialEq, Eq, Hash, FromPrimitive, Clone, Copy)]
    enum Level {
        Easy,
        Normal,
        Hard,
        Oni,
        Ura,
    }

    const LEVELS: [Level; 5] = [
        Level::Easy,
        Level::Normal,
        Level::Hard,
        Level::Oni,
        Level::Ura,
    ];

    pub(crate) fn table_to_songs(table: &table::Table) -> impl Iterator<Item = Song> + '_ {
        table
            .body
            .iter()
            .scan(Option::<Genre>::None, |bef, row| match row.0.len() {
                1 => {
                    *bef = Some(row_to_genre(row));
                    Some(None)
                }
                10 => Some(Some(row_to_song_entry(bef.to_owned().unwrap(), row))),
                _ => panic!(),
            })
            .flatten()
    }

    fn row_to_genre(row: &table::Row) -> Genre {
        if_chain! {
            if let [ table::Cell { contents: InlineElements(ref elements), .. } ] = row.0[..];
            if let [ IE::Strong(InlineElements(ref elements)), IE::Anchor {..} ] = elements[..];
            if let [ IE::Text(ref text) ] = elements[..];
            then {
                Genre { title: text.to_string() }
            } else {
                panic!();
            }
        }
    }

    fn row_to_song_entry(genre: Genre, row: &table::Row) -> Song {
        let (title, others) = {
            let mut it = row.0[3].contents.0.iter().peekable();
            let mut title = String::new();
            while let Some(IE::Strong(InlineElements(text))) = it.peek() {
                if let [IE::Text(ref text)] = text[..] {
                    title += text;
                    it.next();
                } else {
                    dbg!(text);
                    panic!();
                }
            }
            if title.is_empty() {
                panic!("No strong found, or empty title");
            }
            (title, it)
        };

        let subtitle = match others.take(4).collect_vec()[..] {
            [] => None,
            [IE::Br, IE::Text(suc), IE::UnknownElement { text, .. }] if suc == "　　" => {
                Some(text.to_owned())
            }
            ref elements => panic!("Elements go on like {:?}...", elements),
        };
        let scores = LEVELS
            .iter()
            .zip(row.0[5..10].iter())
            .filter_map(cell_to_score)
            .collect::<HashMap<_, _>>();

        Song {
            genre,
            title,
            subtitle,
            scores,
        }
    }

    fn cell_to_score((level, cell): (&Level, &table::Cell)) -> Option<(Level, Score)> {
        lazy_static! {
            static ref STAR_PATTERN: Regex = Regex::new("^★×([1-9]|10)$").unwrap();
        }

        let table::Cell {
            contents: InlineElements(elements),
            ..
        } = cell;
        if let [IE::Text(ref t)] = elements[..] {
            assert_eq!(t, "-");
            return None;
        }
        let (page_title, stars) = if_chain! {
            if let IE::Link { href: LinkType::WikiPage(ref page_title), contents: InlineElements(ref contents) } = elements[0];
            if let IE::Text(ref star) = contents[0];
            then {
                let captures = STAR_PATTERN.captures(star).unwrap();
                let stars = (&captures[1]).parse::<u8>().unwrap();
                (page_title.to_owned(), stars)
            } else {
                panic!();
            }
        };
        let (has_branches, has_papamama_support) = match elements[1..] {
            [] => (false, false),
            [IE::Br, IE::UnknownElement { ref text, .. }] => match text.as_ref() {
                "譜面分岐" => (true, false),
                "(S)" => (true, false),
                _ => panic!("{}", text),
            },
            ref elements => panic!("{:?}", elements),
        };
        Some((
            level.to_owned(),
            Score {
                page_title,
                stars,
                has_branches,
                has_papamama_support,
            },
        ))
    }
}

async fn get_wiki<P>(
    decoder: &mut encoding_rs::Decoder,
    reqwest_client: &reqwest::Client,
    path: P,
) -> Result<Document, Box<dyn error::Error>>
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
    let document = Html::parse_document(&song_list_page.text().await?);
    let parse_result = Document::parse(&document);
    Ok(parse_result)
}
