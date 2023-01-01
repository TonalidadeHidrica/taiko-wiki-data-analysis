#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum HtmlCharcodeError {
    Replace,
    NonCharacter(char),
    ControlCharacter(char),
}
pub fn html_charcode(charcode: u32) -> Result<char, HtmlCharcodeError> {
    use HtmlCharcodeError::*;
    match charcode {
        0 => Err(Replace),                                   // null
        x if x > 0x10FFFF => Err(Replace),                   // replace
        x if (0xD800..=0xDFFF).contains(&x) => Err(Replace), // Surrogate
        x if (0xFDD0..=0xFDEF).contains(&x) => Err(Replace), // Non-character
        0xFFFE | 0xFFFF | 0x1FFFE | 0x1FFFF | 0x2FFFE | 0x2FFFF | 0x3FFFE | 0x3FFFF | 0x4FFFE
        | 0x4FFFF | 0x5FFFE | 0x5FFFF | 0x6FFFE | 0x6FFFF | 0x7FFFE | 0x7FFFF | 0x8FFFE
        | 0x8FFFF | 0x9FFFE | 0x9FFFF | 0xAFFFE | 0xAFFFF | 0xBFFFE | 0xBFFFF | 0xCFFFE
        | 0xCFFFF | 0xDFFFE | 0xDFFFF | 0xEFFFE | 0xEFFFF | 0xFFFFE | 0xFFFFF | 0x10FFFE
        | 0x10FFFF => Err(NonCharacter(char::from_u32(charcode).unwrap())), // Non-character
        0x0D => Err(ControlCharacter(char::from_u32(charcode).unwrap())), // CR
        x if (0x00..=0x1F).contains(&x) || (0x7F..0x9F).contains(&x) => {
            // ASCII control sequence
            Ok(char::from_u32(match x {
                0x09 | 0x0A | 0x0C | 0x0D | 0x20 => x, // ASCII whitespace
                0x80 => 0x20AC,
                0x82 => 0x201A,
                0x83 => 0x0192,
                0x84 => 0x201E,
                0x85 => 0x2026,
                0x86 => 0x2020,
                0x87 => 0x2021,
                0x88 => 0x02C6,
                0x89 => 0x2030,
                0x8A => 0x0160,
                0x8B => 0x2039,
                0x8C => 0x0152,
                0x8E => 0x017D,
                0x91 => 0x2018,
                0x92 => 0x2019,
                0x93 => 0x201C,
                0x94 => 0x201D,
                0x95 => 0x2022,
                0x96 => 0x2013,
                0x97 => 0x2014,
                0x98 => 0x02DC,
                0x99 => 0x2122,
                0x9A => 0x0161,
                0x9B => 0x203A,
                0x9C => 0x0153,
                0x9E => 0x017E,
                0x9F => 0x0178,
                _ => return Err(ControlCharacter(char::from_u32(x).unwrap())),
            })
            .unwrap())
        }
        x => Ok(char::from_u32(x).unwrap()),
    }
}
