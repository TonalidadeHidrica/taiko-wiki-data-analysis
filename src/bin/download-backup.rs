extern crate reqwest;

use async_std::task;
use futures::StreamExt;
use itertools::iterate;
use scraper::{Html, Selector};
use std::fmt;
use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::time::Duration;
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
    let backup_dir = dump_dir.join("backup");
    fs::create_dir_all(&backup_dir)?;

    futures::stream::iter(file_entries.iter().map(|file_entry| {
        download_wiki_entry(&reqwest_client, &wiki_dir, &backup_dir, file_entry)
    }))
    .buffer_unordered(8)
    .collect::<Vec<_>>()
    .await;

    Ok(())
}

#[allow(dead_code)]
async fn sample_download(
    reqwest_client: &reqwest::Client,
    wiki_dir: &Path,
    backup_dir: &Path,
) -> Result<(), Box<dyn error::Error>> {
    download_wiki_entry(
        reqwest_client,
        wiki_dir,
        backup_dir,
        &WikiFileEntry {
            file_name: "C2C0B8DDA4CEC3A3BFCD20BFB7E3FEC2CEA4CEBCFDCFBFB6CA.txt".to_string(),
            last_update: "".to_string(),
            title: "".to_string(),
        },
    )
    .await
}

#[derive(Debug)]
struct ManyRequestErrors(Vec<reqwest::Error>);

impl fmt::Display for ManyRequestErrors {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Many many request resulted in nothing but a pile of errors..."
        )
    }
}

impl error::Error for ManyRequestErrors {
    fn source(&self) -> Option<&(dyn error::Error + 'static)> {
        // self.0.last()
        None
    }
}

async fn send_request_with_retry<F>(
    request_builder_generator: F,
) -> Result<reqwest::Response, ManyRequestErrors>
where
    F: Fn() -> reqwest::RequestBuilder,
{
    let mut errors = Vec::new();

    for timeout in iterate(1000, |x| x * 2).take(5) {
        let response = request_builder_generator().send().await;
        match response {
            Ok(response) => {
                task::sleep(Duration::from_millis(100)).await;
                return Ok(response);
            }
            Err(error) => errors.push(error),
        }
        println!("Retrying after sleeping {} millis...", timeout);
        task::sleep(Duration::from_millis(timeout)).await;
    }

    Err(ManyRequestErrors(errors))
}

async fn get_file_list(
    reqwest_client: &reqwest::Client,
) -> Result<Vec<WikiFileEntry>, Box<dyn error::Error>> {
    let html: String =
        send_request_with_retry(|| reqwest_client.get(URL_PREFIX).query(&[("cmd", "filelist")]))
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

async fn download_wiki_entry(
    reqwest_client: &reqwest::Client,
    wiki_dir: &Path,
    backup_dir: &Path,
    file_entry: &WikiFileEntry,
) -> Result<(), Box<dyn error::Error>> {
    let file_name = &file_entry.file_name;
    let url = URL_PREFIX.to_owned() + "wiki/" + file_name;
    let dest = wiki_dir.join(file_name);
    get_and_save_to_file(reqwest_client, url, dest).await?;

    let backup_file_name = Path::new(file_name).with_extension("gz");
    let backup_file_name = backup_file_name.to_str().unwrap();
    let url = URL_PREFIX.to_owned() + "backup/" + backup_file_name;
    let dest = backup_dir.join(backup_file_name);
    get_and_save_to_file(reqwest_client, url, dest).await?;

    Ok(())
}

async fn get_and_save_to_file<P>(
    reqwest_client: &reqwest::Client,
    url: String,
    dest: P,
) -> Result<(), Box<dyn error::Error>>
where
    P: AsRef<Path>,
{
    // println!("Saving {} into {}", url, dest.as_ref().display());
    let response = send_request_with_retry(|| reqwest_client.get(&url)).await?;
    let mut dest_file = File::create(dest)?;
    dest_file.write_all(&response.bytes().await?)?;

    Ok(())
}
