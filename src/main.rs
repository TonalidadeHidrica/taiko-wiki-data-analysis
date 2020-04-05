extern crate reqwest;

use scraper::{Html, Selector};
use std::error;

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

    get_file_list(reqwest_client).await?;

    Ok(())
}

async fn get_file_list(
    reqwest_client: reqwest::Client,
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
