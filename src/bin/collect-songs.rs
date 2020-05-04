extern crate reqwest;

use encoding_rs::DecoderResult;
use itertools::Itertools;
use scraper::{ElementRef, Html, Selector};
use std::error;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[tokio::main]
async fn main() -> Result<(), Box<dyn error::Error>> {
    let reqwest_client: reqwest::Client = reqwest::Client::new();
    let mut decoder = encoding_rs::EUC_JP.new_decoder();

    let wiki_dir = Path::new("dump-data").join("wiki");
    let path =
        wiki_dir.join("C2C0B8DDA4CEC3A3BFCD20BFB7E3FEC2CEA4CEBCFDCFBFB6CA2FA5B0A5EAA1BCA5F3.txt");
    let song_list_page = get_wiki(&mut decoder, &reqwest_client, path).await?;

    let rows = song_list_page
        .select(&Selector::parse("table > tbody").unwrap())
        .max_by_key(|x| x.descendants().count())
        .unwrap()
        .select(&Selector::parse(":scope > tr").unwrap())
        .collect_vec();

    let contents = {
        let header_predicate = |x: &ElementRef| x.children().count() == 1;
        let headers = rows.iter().filter(|x| header_predicate(x));
        let mut contents = rows.split(header_predicate);
        assert_eq!(contents.next().unwrap(), &[]);
        headers
            .zip(contents)
            .flat_map(|(x, y)| y.iter().map(move |z| (x, z)))
            .collect_vec()
    };
    for (genre, row) in contents {
        let genre_title: String = genre.text().collect_vec().join("");

        let cells = row
            .select(&Selector::parse(":scope > td").unwrap())
            .collect_vec();
        assert_eq!(cells.len(), 10);

        let title_cell = cells.get(3).unwrap();
        let elements = title_cell
            .children()
            .map(|x| ElementRef::wrap(x).ok_or(x))
            .collect_vec();
        assert!(elements.iter().filter_map(|x| x.err()).all(|x| x
            .value()
            .as_text()
            .map_or(false, |y| y.text.to_string().trim().is_empty())));
        let elements = elements
            .iter()
            .filter_map(|x| x.ok().map(|x| x))
            .collect_vec();
        let title_element = elements.get(0).unwrap();
        assert!(Selector::parse("strong").unwrap().matches(title_element));
        let mut title_texts = title_element
            .children()
            .map(|x| x.value().as_text().map(|x| x.text.to_string()));
        let song_title = title_texts.next().unwrap().unwrap();
        assert!(title_texts.next().is_none());

        println!("{}\t{}", genre_title, song_title);
    }

    Ok(())
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
