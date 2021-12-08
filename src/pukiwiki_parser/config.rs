#[derive(Debug)]
pub struct Config {
    pub disable_multiline_plugin: bool,
    pub disable_inline_image_from_uri: bool,
    pub preformat_ltrim: bool,
}
impl Default for Config {
    fn default() -> Self {
        // Default value in pukiwiki.ini.php
        Self {
            disable_multiline_plugin: true,
            disable_inline_image_from_uri: false,
            preformat_ltrim: true,
        }
    }
}
impl Config {
    pub fn taiko_wiki() -> Self {
        Self {
            disable_multiline_plugin: false,
            ..Default::default()
        }
    }
}
