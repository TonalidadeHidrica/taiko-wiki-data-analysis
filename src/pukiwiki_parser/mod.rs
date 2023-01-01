pub mod block;
mod config;
pub mod inline;
mod php;
mod preprocess;
pub mod reader;
mod str_ext;
pub mod token;

pub use block::parse;
pub use config::ParserConfig;
pub use preprocess::PreprocessedString;
