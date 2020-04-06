extern crate reqwest;

use scraper::{Html, Selector};
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::{error, fs};

const URL_PREFIX: &str = "http://www.wikihouse.com/taiko/";

#[derive(Debug)]
struct WikiFileEntry {
    title: String,
    last_update: String,
    file_name: String,
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn error::Error>> {
    let reqwest_client = reqwest::Client::new();

    let file_entries = get_file_list(&reqwest_client).await?;

    let dump_dir = Path::new("dump-data");
    fs::create_dir_all(&dump_dir)?;
    let wiki_dir = dump_dir.join("wiki");
    fs::create_dir_all(&wiki_dir)?;

    for file_entry in file_entries {
        download_file(&reqwest_client, &wiki_dir, file_entry).await?;
    }

    Ok(())
}

async fn get_file_list(
    reqwest_client: &reqwest::Client,
) -> Result<Vec<WikiFileEntry>, reqwest::Error> {
    let html: String = reqwest_client
        .get(URL_PREFIX)
        .query(&[("cmd", "filelist")])
        .send()
        .await?
        .text()
        .await?;
    let document = Html::parse_document(&html);

    Ok(document
        .select(&Selector::parse("div.container div.body > ul > li > ul > li").unwrap())
        .map(|element| {
            // let a_element = element.select_first("a");
            let a_element = element
                .select(&Selector::parse("a").unwrap())
                .next()
                .unwrap();
            let small_element = element
                .select(&Selector::parse("small").unwrap())
                .next()
                .unwrap();
            let li_element = element
                .select(&Selector::parse("ul > li").unwrap())
                .next()
                .unwrap();

            let title = a_element.text().collect::<Vec<_>>().join("");
            let last_update = small_element.text().collect::<Vec<_>>().join("");
            let file_name = li_element.text().collect::<Vec<_>>().join("");

            WikiFileEntry {
                title,
                last_update,
                file_name,
            }
        })
        .collect())
}

async fn download_file(
    reqwest_client: &reqwest::Client,
    wiki_dir: &Path,
    file_entry: WikiFileEntry,
) -> Result<(), Box<dyn error::Error>> {
    let url = URL_PREFIX.to_owned() + "wiki/" + &file_entry.file_name;
    let dest = wiki_dir.join(&file_entry.file_name);
    println!("{:?} {:?}", url, dest);

    let response = reqwest_client.get(&url).send().await?;
    let mut dest_file = File::create(dest)?;
    dest_file.write_all(&response.bytes().await?)?;

    Ok(())
}
