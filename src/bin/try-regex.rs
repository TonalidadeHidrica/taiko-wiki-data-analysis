use enum_set::EnumSet;
use pcre::Pcre;
use regex::Regex;

fn main() {
    let haystack = "日本語 This is a ((test with ((nested footnotes)))).";
    let pattern = r"\w+";
    dbg!(Regex::new(pattern).unwrap().captures(haystack));

    let mut regex = Pcre::compile_with_options(pattern, &EnumSet::new()).unwrap();
    let exec = regex.exec(haystack).unwrap();
    dbg!(exec.group(0));
}
