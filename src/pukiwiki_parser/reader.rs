use std::{
    borrow::Borrow,
    io::{self, BufReader, Read},
    path::PathBuf,
};

use encoding_rs::EUC_JP;
use fs_err::File;
use thiserror::Error;

use super::{block, parse, ParserConfig, PreprocessedString};

pub struct ReaderConfig {
    pub root_dir: PathBuf,
    pub parser_config: ParserConfig,
}
impl ReaderConfig {
    pub fn wiki_dir(&self) -> PathBuf {
        self.root_dir.join("wiki")
    }
}

pub struct WikiReader<C = ReaderConfig> {
    config: C,
    buffer: PreprocessedString,
}
impl<C: Borrow<ReaderConfig>> WikiReader<C> {
    pub fn new(config: C) -> Self {
        Self {
            config,
            buffer: Default::default(),
        }
    }

    pub fn read(&mut self, page_title: impl AsRef<str>) -> Result<Vec<block::Element>, ReadError> {
        let page_title = page_title.as_ref();
        let (page_title_encoded, encoding, replace) = EUC_JP.encode(page_title);
        if encoding != EUC_JP || replace {
            return Err(ReadError::UnencodablePageTitle(page_title.to_owned()));
        }

        let page_title = hex::encode_upper(page_title_encoded) + ".txt";
        let path = self.config.borrow().wiki_dir().join(page_title);
        let mut file = BufReader::new(File::open(&path)?);

        let mut buf = Vec::new();
        let _ = file.read_to_end(&mut buf)?;
        let (decoded, encoding, replace) = EUC_JP.decode(&buf);
        if encoding != EUC_JP || replace {
            return Err(ReadError::DecodeFailed(path));
        }

        self.buffer = FromIterator::from_iter(decoded.chars());
        Ok(parse(&self.config.borrow().parser_config, &self.buffer))
    }
}

#[derive(Debug, Error)]
pub enum ReadError {
    #[error("Cannot open file: page title {0:?} is unencodable to EUC-JP")]
    UnencodablePageTitle(String),
    #[error("Cannot open file: {0:?}")]
    FailedToOpenFile(#[from] io::Error),
    #[error("File {0:?} was not encodedd in EUC_JP")]
    DecodeFailed(PathBuf),
}
