#[derive(Debug)]
pub struct ParserConfig {
    pub disable_multiline_plugin: bool,
    pub disable_inline_image_from_uri: bool,
    pub preformat_ltrim: bool,
}
impl Default for ParserConfig {
    fn default() -> Self {
        // Default value in pukiwiki.ini.php
        Self {
            disable_multiline_plugin: true,
            disable_inline_image_from_uri: false,
            preformat_ltrim: true,
        }
    }
}
impl ParserConfig {
    pub fn taiko_wiki() -> Self {
        Self {
            disable_multiline_plugin: false,
            ..Default::default()
        }
    }
}
