extern crate reqwest;

use scraper::{Html, Selector};

const URL_PREFIX: &str = "http://www.wikihouse.com/taiko/";

#[tokio::main]
async fn main() -> Result<(), reqwest::Error> {
    let html: String = reqwest::Client::new()
        .get(URL_PREFIX)
        .query(&[("cmd", "filelist")])
        .send().await?
        .text().await?;
    let document = Html::parse_document(&html);

    for element in document.select(&Selector::parse("div.container div.body > ul > li > ul > li").unwrap()) {
        let a_element = element.select(&Selector::parse("a").unwrap()).next().unwrap();
        let small_element = element.select(&Selector::parse("small").unwrap()).next().unwrap();
        let li_element = element.select(&Selector::parse("ul > li").unwrap()).next().unwrap();

        let title = a_element.text().collect::<Vec<_>>().join("");
        let last_update = small_element.text().collect::<Vec<_>>().join("");
        let file_name = li_element.text().collect::<Vec<_>>().join("");

        println!("{}\t{}\t{}", title, last_update, file_name);
    }

    Ok(())
}
