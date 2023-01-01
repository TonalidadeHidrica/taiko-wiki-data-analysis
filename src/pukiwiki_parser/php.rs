use crate::regex;

pub fn as_numeric(val: &str) -> Option<f64> {
    // https://www.php.net/manual/en/language.types.numeric-strings.php
    // WHITESPACES      \s*
    // LNUM             [0-9]+
    // DNUM             ([0-9]*)[\.]{LNUM}) | ({LNUM}[\.][0-9]*)
    // EXPONENT_DNUM    (({LNUM} | {DNUM}) [eE][+-]? {LNUM})
    // INT_NUM_STRING   {WHITESPACES} [+-]? {LNUM} {WHITESPACES}
    // FLOAT_NUM_STRING {WHITESPACES} [+-]? ({DNUM} | {EXPONENT_DNUM}) {WHITESPACES}
    // NUM_STRING       ({INT_NUM_STRING} | {FLOAT_NUM_STRING})
    let val = val.trim_start();
    let is_int = regex!(r"[+-]?[0-9]+").is_match(val);
    let is_float = regex!(
        r"^[+-]?([0-9]*\.[0-9]+|[0-9]+\.[0-9]*|([0-9]+|[0-9]*\.[0-9]+|[0-9]+\.[0-9]*)[eE][+-][0-9]+)$"
    )
    .is_match(val);
    (is_int || is_float).then(|| val.parse().ok()).flatten()
}

#[cfg(test)]
mod tests {
    use super::as_numeric;

    #[test]
    fn test_as_numeric() {
        assert_eq!(as_numeric("100"), Some(100.0));
        assert_eq!(as_numeric("+100"), Some(100.0));
        assert_eq!(as_numeric("-100"), Some(-100.0));

        assert_eq!(as_numeric(" 100"), Some(100.0));
        assert_eq!(as_numeric("    +100"), Some(100.0));
        assert_eq!(as_numeric(" \t\t\n\r-100"), Some(-100.0));

        assert_eq!(as_numeric("  13."), Some(13.));
        assert_eq!(as_numeric(" +13.75"), Some(13.75));
        assert_eq!(as_numeric(" -.75"), Some(-0.75));

        assert_eq!(as_numeric(" 4e3"), Some(4e3));
        assert_eq!(as_numeric(" -2.e+3"), Some(-2e3));
        assert_eq!(as_numeric(" +2.e-1"), Some(2e-1));

        assert_eq!(as_numeric("."), None);
        assert_eq!(as_numeric(" 3 pigs"), None);
        assert_eq!(as_numeric("There are 5 dogs"), None);
        assert_eq!(as_numeric("5 years"), None);
    }
}
