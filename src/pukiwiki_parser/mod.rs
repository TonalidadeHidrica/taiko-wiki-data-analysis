pub mod block;
pub mod config;
pub mod inline;
mod php;
pub mod preprocess;
mod str_ext;
pub mod token;

pub use block::parse;
pub use config::Config;
