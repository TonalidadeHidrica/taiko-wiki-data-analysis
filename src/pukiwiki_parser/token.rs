use std::{collections::HashMap, iter::Peekable, ops::Range, str::FromStr};

use either::*;
use entities::{Entity, ENTITIES};
use itertools::{any, zip, Itertools};
use len_trait::Len;
use loop_unwrap::unwrap_break;
use once_cell::sync::Lazy;
use regex::Regex;

use crate::{
    html5_spec::numeric_character_reference::{html_charcode, HtmlCharcodeError},
    my_itertools::{MyItertools, PeekableExt},
    regex,
    regex_ext::iter::{MatchComponent, MatchIterator, MatchLike},
};

use super::str_ext::{
    self, find_iter_char, find_iter_char_any, find_iter_str, TwoStr, TwoStrConcatRef,
};

#[derive(Debug, derive_more::From)]
pub enum InlineToken<'a> {
    Str(&'a str),
    SpecialChar(SpecialChar),
    NewLine,
    FaceMark(FaceMark),
    PushButton(PushButton),
    MobileEmoji(MobileEmoji),
    StyleSpecifierStart(StyleSpecifierStart),
    #[from(ignore)]
    StyleStart(StyleKind),
    #[from(ignore)]
    StyleEnd(StyleKind),
}
#[derive(Debug, derive_more::From)]
pub enum SpecialChar {
    NamedEntity(&'static Entity),
    Char(char),
    #[from(ignore)]
    NonCharacter(char),
    #[from(ignore)]
    ControlCharacter(char),
    ReplacementCharacter,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum FaceMark {
    Smile,
    BigSmile,
    Huh,
    Oh,
    Wink,
    Sad,
    Heart,
    Worried,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum PushButton {
    Pb0,
    Pb1,
    Pb2,
    Pb3,
    Pb4,
    Pb5,
    Pb6,
    Pb7,
    Pb8,
    Pb9,
    PbHash,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum MobileEmoji {
    Zzz,
    Man,
    Clock,
    Mail,
    MailTo,
    Phone,
    PhoneTo,
    FaxTo,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum StyleSpecifierStart {
    ColorBlock,
    SizeBlock,
    ColorSwitch,
    SizeSwitch,
}
#[derive(PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum StyleKind {
    Span,
    Ins,
    Del,
    Em,
    Strong,
}
pub(crate) fn make_line_rules<'a: 'ret, 'o: 'ret, 'ret>(
    str: impl Into<TwoStrConcatRef<'a, 'o>>,
) -> impl Iterator<Item = InlineToken<'o>> + 'ret {
    let str = str.into();
    let regex = regex!(
        r"(?x)
        &(?:
            \#(?P<entity_decimal>[0-9]+)
            |
            \#x(?P<entity_hex>[0-9a-f]+)
            |
            (?P<entity_named>A(?:Elig|acute|circ|grave|lpha|ring|tilde|uml)|Beta|C(?:cedil|hi)|D(?:agger|elta)|E(?:TH|acute|circ|grave|psilon|ta|uml)|Gamma|I(?:acute|circ|grave|ota|uml)|Kappa|Lambda|Mu|N(?:tilde|u)|O(?:Elig|acute|circ|grave|m(?:ega|icron)|slash|tilde|uml)|P(?:hi|i|rime|si)|Rho|S(?:caron|igma)|T(?:HORN|au|heta)|U(?:acute|circ|grave|psilon|uml)|Xi|Y(?:acute|uml)|Zeta|a(?:acute|c(?:irc|ute)|elig|grave|l(?:efsym|pha)|mp|n(?:d|g)|pos|ring|symp|tilde|uml)|b(?:dquo|eta|rvbar|ull)|c(?:ap|cedil|e(?:dil|nt)|hi|irc|lubs|o(?:ng|py)|rarr|u(?:p|rren))|d(?:Arr|a(?:gger|rr)|e(?:g|lta)|i(?:ams|vide))|e(?:acute|circ|grave|m(?:pty|sp)|nsp|psilon|quiv|t(?:a|h)|u(?:ml|ro)|xist)|f(?:nof|orall|ra(?:c(?:1(?:2|4)|34)|sl))|g(?:amma|e|t)|h(?:Arr|arr|e(?:arts|llip))|i(?:acute|circ|excl|grave|mage|n(?:fin|t)|ota|quest|sin|uml)|kappa|l(?:Arr|a(?:mbda|ng|quo|rr)|ceil|dquo|e|floor|o(?:wast|z)|rm|s(?:aquo|quo)|t)|m(?:acr|dash|i(?:cro|ddot|nus)|u)|n(?:abla|bsp|dash|e|i|ot(?:in)?|sub|tilde|u)|o(?:acute|circ|elig|grave|line|m(?:ega|icron)|plus|r(?:d(?:f|m))?|slash|ti(?:lde|mes)|uml)|p(?:ar(?:a|t)|er(?:mil|p)|hi|i(?:v)?|lusmn|ound|r(?:ime|o(?:d|p))|si)|quot|r(?:Arr|a(?:dic|ng|quo|rr)|ceil|dquo|e(?:al|g)|floor|ho|lm|s(?:aquo|quo))|s(?:bquo|caron|dot|ect|hy|i(?:gma(?:f)?|m)|pades|u(?:b(?:e)?|m|p(?:1|2|3|e)?)|zlig)|t(?:au|h(?:e(?:re4|ta(?:sym)?)|insp|orn)|i(?:lde|mes)|rade)|u(?:Arr|a(?:cute|rr)|circ|grave|ml|psi(?:h|lon)|uml)|weierp|xi|y(?:acute|en|uml)|z(?:eta|w(?:j|nj)))
        );
        | (?P<newline> \r)

        # face marks
        | (?P<face_smile0>      \s:\)       )
        | (?P<face_bigsmile0>   \s:D        )
        | (?P<face_huh0>        \s:p        )
        | (?P<face_huh1>        \s:d        )
        | (?P<face_oh0>         \sXD        )
        | (?P<face_oh1>         \sX\(       )
        | (?P<face_wink0>       \s;\)       )
        | (?P<face_sad0>        \s;\(       )
        | (?P<face_sad1>        \s:\(       )

        # face marks (amp)
        | (?P<face_smile1>      &smile;     )
        | (?P<face_bigsmile1>   &bigsmile;  )
        | (?P<face_huh2>        &huh;       )
        | (?P<face_oh2>         &oh;        )
        | (?P<face_wink1>       &wink;      )
        | (?P<face_sad2>        &sad;       )
        | (?P<face_heart0>      &heart;     )
        | (?P<face_worried0>    &worried;   )

        # face marks (japanese)
        | (?P<face_smile2>      \s\(\^\^\)  )
        | (?P<face_bigsmile2>   \s\(\^-\^   )
        | (?P<face_oh3>         \s\(\.\.;   )
        | (?P<face_wink2>       \s\(\^_-\)  )
        | (?P<face_sad3>        \s\(--;     )
        | (?P<face_worried1>    \s\(\^\^;\) )
        | (?P<face_worried2>    \s\(\^\^;   )

        # mobile push buttons
        | (?P<mobile_1>         &pb1;       )
        | (?P<mobile_2>         &pb2;       )
        | (?P<mobile_3>         &pb3;       )
        | (?P<mobile_4>         &pb4;       )
        | (?P<mobile_5>         &pb5;       )
        | (?P<mobile_6>         &pb6;       )
        | (?P<mobile_7>         &pb7;       )
        | (?P<mobile_8>         &pb8;       )
        | (?P<mobile_9>         &pb9;       )
        | (?P<mobile_0>         &pb0;       )
        | (?P<mobile_h>         &pb\#;      )

        # mobile emojis
        | (?P<amp_zzz>          &zzz;       )
        | (?P<amp_man>          &man;       )
        | (?P<amp_clock>        &clock;     )
        | (?P<amp_mail>         &mail;      )
        | (?P<amp_mailto>       &mailto;    )
        | (?P<amp_phone>        &phone;     )
        | (?P<amp_phoneto>      &phoneto;   )
        | (?P<amp_faxto>        &faxto;     )
    "
    );

    let color_blocks = get_color_blocks(str);
    let size_blocks = get_size_blocks(str, &color_blocks);
    let color_switches = get_color_switches(str, &color_blocks, &size_blocks);
    let size_switches = get_size_switches(str, &color_blocks, &size_blocks, &color_switches);
    let (inses, dels) = get_format_ranges(str, regex!("%{2,}"));
    let (ems, strongs) = get_format_ranges(str, regex!("'{2,}"));

    use InlineToken::{StyleEnd, StyleStart};
    use StyleKind::*;
    use StyleSpecifierStart::*;

    // TODO [perf] due to the lifetime constraint, we have to temporarily store this in a vec
    let other_matches = str
        .regex_captures_iter(regex)
        .remove_overlapping(color_blocks.iter().flat_map(|x| [&x.kw, &x.mid, &x.close]))
        .remove_overlapping(size_blocks.iter().flat_map(|x| [&x.kw, &x.mid, &x.close]))
        .remove_overlapping(color_switches.iter().flat_map(|x| [&x.kw, &x.delim]))
        .remove_overlapping(size_switches.iter().flat_map(|x| [&x.kw, &x.delim]))
        .remove_overlapping(inses.iter().flat_map(|x| [&x.start, &x.end]))
        .remove_overlapping(dels.iter().flat_map(|x| [&x.start, &x.end]))
        .remove_overlapping(ems.iter().flat_map(|x| [&x.start, &x.end]))
        .remove_overlapping(strongs.iter().flat_map(|x| [&x.start, &x.end]))
        .collect_vec();

    // lexicographical order of the first element
    let cmp =
        |x: &(Range<usize>, _), y: &(Range<usize>, _)| (x.0.start, x.0.end) < (y.0.start, y.0.end);

    other_matches
        .into_iter()
        .map(|x| (x.start_pos()..x.end_pos(), match_to_token(x)))
        .merge_by(
            color_blocks.into_iter().flat_map(|x| {
                [
                    (x.kw, ColorBlock.into()),
                    (x.mid, StyleStart(Span)),
                    (x.close, StyleEnd(Span)),
                ]
            }),
            cmp,
        )
        .merge_by(
            size_blocks.into_iter().flat_map(|x| {
                [
                    (x.kw, SizeBlock.into()),
                    (x.mid, StyleStart(Span)),
                    (x.close, StyleEnd(Span)),
                ]
            }),
            cmp,
        )
        .merge_by(
            color_switches.into_iter().flat_map(|x| {
                [
                    (x.kw, ColorSwitch.into()),
                    (x.delim, StyleStart(Span)),
                    (x.end..x.end, StyleEnd(Span)),
                ]
            }),
            cmp,
        )
        .merge_by(
            size_switches.into_iter().flat_map(|x| {
                [
                    (x.kw, SizeSwitch.into()),
                    (x.delim, StyleStart(Span)),
                    (x.end..x.end, StyleEnd(Span)),
                ]
            }),
            cmp,
        )
        .merge_by(
            inses
                .into_iter()
                .flat_map(|x| [(x.start, StyleStart(Ins)), (x.end, StyleEnd(Ins))]),
            cmp,
        )
        .merge_by(
            dels.into_iter()
                .flat_map(|x| [(x.start, StyleStart(Del)), (x.end, StyleEnd(Del))]),
            cmp,
        )
        .merge_by(
            ems.into_iter()
                .flat_map(|x| [(x.start, StyleStart(Em)), (x.end, StyleEnd(Em))]),
            cmp,
        )
        .merge_by(
            strongs
                .into_iter()
                .flat_map(|x| [(x.start, StyleStart(Strong)), (x.end, StyleEnd(Strong))]),
            cmp,
        )
        .match_components(str, str.len())
        .flat_map(|x| match x {
            MatchComponent::Between(s) => Left(TwoStr::from(s).into_iter().map(InlineToken::Str)),
            MatchComponent::Match((_, token)) => Right([token].into_iter()),
        })
}

impl<T> MatchLike for (Range<usize>, T) {
    fn start_pos(&self) -> usize {
        self.0.start
    }
    fn end_pos(&self) -> usize {
        self.0.end
    }
}

fn match_to_token<'o>(captures: str_ext::regex::Captures<'_, 'o>) -> InlineToken<'o> {
    use FaceMark::*;
    use InlineToken as IT;
    use MobileEmoji::*;
    use PushButton::*;

    static ENTITY_MAP: Lazy<HashMap<&str, &Entity>> =
        once_cell::sync::Lazy::new(|| ENTITIES.iter().map(|e| (e.entity, e)).collect());

    let as_charcode = |a: Result<u32, _>| match html_charcode(a.unwrap_or(u32::MAX)) {
        Ok(c) => IT::SpecialChar(c.into()),
        Err(HtmlCharcodeError::Replace) => IT::SpecialChar(SpecialChar::ReplacementCharacter),
        Err(HtmlCharcodeError::NonCharacter(c)) => IT::SpecialChar(SpecialChar::NonCharacter(c)),
        Err(HtmlCharcodeError::ControlCharacter(c)) => {
            IT::SpecialChar(SpecialChar::ControlCharacter(c))
        }
    };

    let has_match = |s| captures.name(s).is_some();

    if let Some(entity_decimal) = captures.name("entity_decimal") {
        as_charcode(u32::from_str(entity_decimal.as_str().into()))
    } else if let Some(entity_hex) = captures.name("entity_hex") {
        as_charcode(u32::from_str_radix(entity_hex.as_str().into(), 16))
    } else if has_match("entity_named") {
        let entity = *ENTITY_MAP
            .get(captures.get(0).unwrap().as_str().into())
            .unwrap();
        IT::SpecialChar(entity.into())
    } else if has_match("newline") {
        IT::NewLine
    } else if any(["face_smile0", "face_smile1", "face_smile2"], has_match) {
        Smile.into()
    } else if any(
        ["face_bigsmile0", "face_bigsmile1", "face_bigsmile2"],
        has_match,
    ) {
        BigSmile.into()
    } else if any(["face_huh0", "face_huh1", "face_huh2"], has_match) {
        Huh.into()
    } else if any(["face_oh0", "face_oh1", "face_oh2", "face_oh3"], has_match) {
        Oh.into()
    } else if any(["face_wink0", "face_wink1", "face_wink2"], has_match) {
        Wink.into()
    } else if any(
        ["face_sad0", "face_sad1", "face_sad2", "face_sad3"],
        has_match,
    ) {
        Sad.into()
    } else if has_match("face_heart0") {
        Heart.into()
    } else if any(
        ["face_worried0", "face_worried1", "face_worried2"],
        has_match,
    ) {
        Worried.into()
    } else {
        match () {
            _ if has_match("mobile_0") => Pb0.into(),
            _ if has_match("mobile_1") => Pb1.into(),
            _ if has_match("mobile_2") => Pb2.into(),
            _ if has_match("mobile_3") => Pb3.into(),
            _ if has_match("mobile_4") => Pb4.into(),
            _ if has_match("mobile_5") => Pb5.into(),
            _ if has_match("mobile_6") => Pb6.into(),
            _ if has_match("mobile_7") => Pb7.into(),
            _ if has_match("mobile_8") => Pb8.into(),
            _ if has_match("mobile_9") => Pb9.into(),
            _ if has_match("mobile_h") => PbHash.into(),
            _ if has_match("amp_zzz") => Zzz.into(),
            _ if has_match("amp_man") => Man.into(),
            _ if has_match("amp_clock") => Clock.into(),
            _ if has_match("amp_mail") => Mail.into(),
            _ if has_match("amp_mailto") => MailTo.into(),
            _ if has_match("amp_phone") => Phone.into(),
            _ if has_match("amp_phoneto") => PhoneTo.into(),
            _ if has_match("amp_faxto") => FaxTo.into(),
            _ => unreachable!("Guarded by regex"),
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct BraceBlock {
    /// `COLOR(` or `SIZE(`
    kw: Range<usize>,
    /// `){`
    mid: Range<usize>,
    /// `}`
    close: Range<usize>,
}
fn get_brace_blocks(
    str: TwoStrConcatRef,
    keyword: &str,
    mut middle_iter: Peekable<impl Iterator<Item = Range<usize>>>,
    mut close_iter: Peekable<impl Iterator<Item = Range<usize>>>,
    mut paren_iter: Peekable<impl Iterator<Item = Range<usize>>>,
) -> Vec<BraceBlock> {
    let mut res = vec![];
    let mut last = 0;
    for kw in find_iter_str(str, keyword) {
        if kw.start < last {
            continue;
        }
        let mid = unwrap_break!(middle_iter.peeking_find(|x| kw.end <= x.start));
        let next_paren = paren_iter
            .peeking_find(|x| kw.end <= x.start)
            .expect("){ is found");
        if next_paren.start < mid.start {
            continue;
        }
        let mid = middle_iter.next().expect("proven by existence of mid"); // Peeked::pop(mid);
        let close = unwrap_break!(close_iter.find(|x| mid.end <= x.start));
        last = close.end;
        res.push(BraceBlock { kw, mid, close });
    }
    res
}
const PARENS: &[char] = &['(', ')'];
fn get_color_blocks<'a, 'o>(str: impl Into<TwoStrConcatRef<'a, 'o>>) -> Vec<BraceBlock> {
    let str = str.into();
    let middle_iter = find_iter_str(str, "){").peekable();
    let close_iter = find_iter_char(str, '}').peekable();
    let paren_iter = find_iter_char_any(str, PARENS).peekable();
    get_brace_blocks(str, "COLOR(", middle_iter, close_iter, paren_iter)
}
fn get_size_blocks<'a, 'o>(
    str: impl Into<TwoStrConcatRef<'a, 'o>>,
    color_blocks: &[BraceBlock],
) -> Vec<BraceBlock> {
    let str = str.into();
    let middle_iter = find_iter_str(str, "){")
        .remove_overlapping(color_blocks.iter().map(|x| &x.mid))
        .peekable();
    let close_iter = find_iter_char(str, '}')
        .remove_overlapping(color_blocks.iter().map(|x| &x.close))
        .peekable();
    let paren_iter = find_iter_char_any(str, PARENS)
        .remove_overlapping(color_blocks.iter().flat_map(|x| [&x.kw, &x.mid]))
        .peekable();
    get_brace_blocks(str, "SIZE(", middle_iter, close_iter, paren_iter)
}

struct FormatSwitch {
    /// `COLOR(` or `SIZE(`
    kw: Range<usize>,
    /// `):`
    delim: Range<usize>,
    /// end
    end: usize,
}
fn get_format_switch(
    str: TwoStrConcatRef,
    keyword: &str,
    mut delim_iter: Peekable<impl Iterator<Item = Range<usize>> + Clone>,
    mut paren_iter: Peekable<impl Iterator<Item = Range<usize>>>,
) -> Vec<FormatSwitch> {
    let n = str.len();
    let keywords = find_iter_str(str, keyword).filter_map(move |kw| {
        let delim = delim_iter.peeking_find(|d| kw.end <= d.start)?;
        Some((kw, delim.clone()))
    });
    let mut keywords_next = keywords.clone().map(|x| x.0.start).peekable();
    let mut res = vec![];
    let mut last = 0;
    for (kw, delim) in keywords {
        if kw.start < last {
            continue;
        }
        let &end = keywords_next
            .peeking_find(|&next| delim.end <= next)
            .unwrap_or(&n);
        let next_paren = paren_iter
            .peeking_find(|x| kw.end <= x.start)
            .expect("): is found");
        if next_paren.start < delim.start {
            continue;
        }
        last = end;
        res.push(FormatSwitch { kw, delim, end });
    }
    res
}
fn get_color_switches<'a, 'o>(
    str: impl Into<TwoStrConcatRef<'a, 'o>>,
    color_blocks: &[BraceBlock],
    size_blocks: &[BraceBlock],
) -> Vec<FormatSwitch> {
    let str = str.into();
    let delim_iter = find_iter_str(str, "):").peekable();
    let paren_iter = find_iter_char_any(str, PARENS)
        .remove_overlapping(color_blocks.iter().flat_map(|x| [&x.kw, &x.mid]))
        .remove_overlapping(size_blocks.iter().flat_map(|x| [&x.kw, &x.mid]))
        .peekable();
    get_format_switch(str, "COLOR(", delim_iter, paren_iter)
}
fn get_size_switches<'a, 'o>(
    str: impl Into<TwoStrConcatRef<'a, 'o>>,
    color_blocks: &[BraceBlock],
    size_blocks: &[BraceBlock],
    color_switches: &[FormatSwitch],
) -> Vec<FormatSwitch> {
    let str = str.into();
    let delim_iter = find_iter_str(str, "):")
        .remove_overlapping(color_switches.iter().map(|x| &x.delim))
        .peekable();
    let paren_iter = find_iter_char_any(str, PARENS)
        .remove_overlapping(color_blocks.iter().flat_map(|x| [&x.kw, &x.mid]))
        .remove_overlapping(size_blocks.iter().flat_map(|x| [&x.kw, &x.mid]))
        .remove_overlapping(color_switches.iter().flat_map(|x| [&x.kw, &x.delim]))
        .peekable();
    get_format_switch(str, "SIZE(", delim_iter, paren_iter)
}

struct FormatRange {
    start: Range<usize>,
    end: Range<usize>,
}
fn get_format_ranges<'a, 'o>(
    str: impl Into<TwoStrConcatRef<'a, 'o>>,
    regex: &Regex,
) -> (Vec<FormatRange>, Vec<FormatRange>) {
    let str = str.into();
    let mut positions = str.regex_find_iter(regex).map(|x| x.range()).collect_vec();
    let mut res = (vec![], vec![]);
    for (res, len) in zip([&mut res.0, &mut res.1], [3, 2]) {
        let find = |slice: &[Range<usize>]| (0..slice.len()).find(|&i| slice[i].len() >= len);
        let mut s = find(&positions);
        while let Some(i) = s {
            let (bef, aft) = positions.split_at_mut(i + 1);
            let bef = &mut bef[i];
            let j = unwrap_break!(find(aft));
            let aft = &mut aft[j];
            res.push(FormatRange {
                start: bef.end - len..bef.end,
                end: aft.start..aft.start + len,
            });
            bef.end -= len;
            aft.start += len;
            s = find(&positions[i + j + 1..]).map(|k| i + j + 1 + k);
        }
    }
    res
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use entities::Entity;

    use super::{
        get_color_blocks, get_color_switches, get_format_ranges, get_size_blocks,
        get_size_switches, make_line_rules, InlineToken as IT,
    };
    use crate::regex;

    macro_rules! assert_blocks {
        ($lhs: expr, [$($rhs: expr),* $(,)?]) => {{
            use ::itertools::Itertools;
            assert_eq!(
                $lhs.into_iter().map(|x| (x.kw, x.mid, x.close)).collect_vec(),
                vec![$($rhs),*]
            );
        }};
    }

    macro_rules! assert_switches {
        ($lhs: expr, [$($rhs: expr),* $(,)?]) => {{
            use ::itertools::Itertools;
            assert_eq!(
                $lhs.into_iter().map(|x| (x.kw, x.delim, x.end)).collect_vec(),
                vec![$($rhs),*]
            );
        }};
    }

    macro_rules! assert_ranges {
        ($lhs: expr, [$($rhs: expr),* $(,)?]) => {{
            use ::itertools::Itertools;
            assert_eq!(
                $lhs.into_iter().map(|x| (x.start, x.end)).collect_vec(),
                vec![$($rhs),*]
            );
        }};
    }

    #[test]
    fn test_color_blocks() {
        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(blue){test}";
        let res = get_color_blocks(str);
        assert_blocks!(res, [(0..6, 10..12, 16..17)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "    COLOR(blue){    { COLOR(red){   {  }}}}";
        let res = get_color_blocks(str);
        assert_blocks!(res, [(4..10, 14..16, 39..40)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "    COLOR( COLOR(green){ test } ){ another test }";
        let res = get_color_blocks(str);
        assert_blocks!(res, [(11..17, 22..24, 30..31)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(red){text}COLOR(blue){another}";
        let res = get_color_blocks(str);
        assert_blocks!(res, [(0..6, 9..11, 15..16), (16..22, 26..28, 35..36)]);
    }

    #[test]
    fn test_size_blocks() {
        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "  SIZE(13){large text}  ";
        let colors = get_color_blocks(str);
        let sizes = get_size_blocks(str, &colors);
        assert_blocks!(colors, []);
        assert_blocks!(sizes, [(2..7, 9..11, 21..22)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "SIZE(13){COLOR(red){text}}";
        let colors = get_color_blocks(str);
        let sizes = get_size_blocks(str, &colors);
        assert_blocks!(colors, [(9..15, 18..20, 24..25)]);
        assert_blocks!(sizes, [(0..5, 7..9, 25..26)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(red){SIZE(13){text}}";
        let colors = get_color_blocks(str);
        let sizes = get_size_blocks(str, &colors);
        assert_blocks!(colors, [(0..6, 9..11, 24..25)]);
        assert_blocks!(sizes, [(11..16, 18..20, 25..26)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "SIZE(COLOR(red){text}){another}";
        let colors = get_color_blocks(str);
        let sizes = get_size_blocks(str, &colors);
        assert_blocks!(colors, [(5..11, 14..16, 20..21)]);
        assert_blocks!(sizes, [(0..5, 21..23, 30..31)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(SIZE(18){text}){another}";
        let colors = get_color_blocks(str);
        let sizes = get_size_blocks(str, &colors);
        assert_blocks!(colors, []);
        assert_blocks!(sizes, [(6..11, 13..15, 19..20)]);
    }

    #[test]
    fn test_color_switches() {
        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "no color COLOR(red):this is a text COLOR(blue):this is another test";
        let color_blocks = get_color_blocks(str);
        let size_blocks = get_size_blocks(str, &color_blocks);
        let color_switches = get_color_switches(str, &color_blocks, &size_blocks);
        assert_blocks!(color_blocks, []);
        assert_blocks!(size_blocks, []);
        assert_switches!(color_switches, [(9..15, 18..20, 35), (35..41, 45..47, 67)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(  COLOR(red):test COLOR( COLOR(green): another  COLOR( ():";
        let color_blocks = get_color_blocks(str);
        let size_blocks = get_size_blocks(str, &color_blocks);
        let color_switches = get_color_switches(str, &color_blocks, &size_blocks);
        assert_blocks!(color_blocks, []);
        assert_blocks!(size_blocks, []);
        assert_switches!(color_switches, [(8..14, 17..19, 24), (31..37, 42..44, 54)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "COLOR(SIZE(COLOR(red){text}){another}):test";
        let color_blocks = get_color_blocks(str);
        let size_blocks = get_size_blocks(str, &color_blocks);
        let color_switches = get_color_switches(str, &color_blocks, &size_blocks);
        assert_blocks!(color_blocks, [(11..17, 20..22, 26..27)]);
        assert_blocks!(size_blocks, [(6..11, 27..29, 36..37)]);
        assert_switches!(color_switches, [(0..6, 37..39, 43)]);
    }

    #[test]
    fn test_size_switches() {
        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "SIZE(COLOR(SIZE(COLOR(red){p}){q}):r):s COLOR(blue):t SIZE(12):u";
        let color_blocks = get_color_blocks(str);
        let size_blocks = get_size_blocks(str, &color_blocks);
        let color_switches = get_color_switches(str, &color_blocks, &size_blocks);
        let size_switches = get_size_switches(str, &color_blocks, &size_blocks, &color_switches);
        assert_blocks!(color_blocks, [(16..22, 25..27, 28..29)]);
        assert_blocks!(size_blocks, [(11..16, 29..31, 32..33)]);
        assert_switches!(color_switches, [(5..11, 33..35, 40), (40..46, 50..52, 64)]);
        assert_switches!(size_switches, [(0..5, 36..38, 54), (54..59, 61..63, 64)]);
    }

    #[test]
    fn test_get_format_ranges() {
        let regex = regex!(r"%{2,}");

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "  %%% %%%    %% %%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(2..5, 6..9)]);
        assert_ranges!(dva, [(13..15, 16..18)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "  %%%%%   %%%%%%%  %%%%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(4..7, 10..13), (14..17, 19..22)]);
        assert_ranges!(dva, []);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "  %%%%%   %%%%%%%  %%%%%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(4..7, 10..13), (14..17, 19..22)]);
        assert_ranges!(dva, [(2..4, 22..24)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "  %%%%%   %%%%%%%%  %%%%%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(4..7, 10..13), (15..18, 20..23)]);
        assert_ranges!(dva, [(2..4, 13..15)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "%% %%% %% %%%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(3..6, 10..13)]);
        assert_ranges!(dva, [(0..2, 7..9)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "%% %%% %%% %%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(3..6, 7..10)]);
        assert_ranges!(dva, [(0..2, 11..13)]);

        //                   1         2         3         4         5         6         7         8
        //         012345678901234567890123456789012345678901234567890123456789012345678901234567890
        let str = "%%% %%%% %%% %%%";
        let (tri, dva) = get_format_ranges(str, regex);
        assert_ranges!(tri, [(0..3, 4..7), (9..12, 13..16)]);
        assert_ranges!(dva, []);
    }

    #[test]
    fn test_make_line_rules_entities() {
        use super::SpecialChar as SC;

        let mut tokens = make_line_rules("&#64;&#2832;&#30906;");
        assert_matches!(tokens.next(), Some(IT::SpecialChar(SC::Char('@'))));
        assert_matches!(tokens.next(), Some(IT::SpecialChar(SC::Char('ଐ'))));
        assert_matches!(tokens.next(), Some(IT::SpecialChar(SC::Char('確'))));
        assert_matches!(tokens.next(), None);

        let mut tokens = make_line_rules(
            "&#x0;&#x86;&#xa7;&#x8a8d;&#xd800;&#x1f34e;&#x7fffe;&#xffff123;&#xeeeeeeeeeeeeeeeeeee;",
        );
        assert_matches!(
            tokens.next(),
            Some(IT::SpecialChar(SC::ReplacementCharacter))
        ); // Null
        assert_matches!(tokens.next(), Some(IT::SpecialChar(SC::Char('†'))));
        assert_matches!(tokens.next(), Some(IT::SpecialChar(SC::Char('§'))));
        assert_matches!(tokens.next(), Some(IT::SpecialChar(SC::Char('認'))));
        assert_matches!(
            tokens.next(),
            Some(IT::SpecialChar(SC::ReplacementCharacter))
        ); // Surrogate
        assert_matches!(tokens.next(), Some(IT::SpecialChar(SC::Char('🍎'))));
        assert_matches!(
            tokens.next(),
            Some(IT::SpecialChar(SC::NonCharacter('\u{7fffe}')))
        );
        assert_matches!(
            tokens.next(),
            Some(IT::SpecialChar(SC::ReplacementCharacter))
        ); // Out of range
        assert_matches!(
            tokens.next(),
            Some(IT::SpecialChar(SC::ReplacementCharacter))
        ); // Out of range (overflow)
        assert_matches!(tokens.next(), None);

        // Unlinke HTML, the parser does not accept capital hex
        let mut tokens = make_line_rules("&#x1F34E;");
        assert_matches!(tokens.next(), Some(IT::Str(_)));
        assert_matches!(tokens.next(), None);

        let mut tokens = make_line_rules("&Aacute&Aacute;&cap;&unknown;");
        assert_matches!(tokens.next(), Some(IT::Str(s)) if s == "&Aacute");
        assert_matches!(
            tokens.next(),
            Some(IT::SpecialChar(SC::NamedEntity(Entity {
                characters: "Á",
                ..
            })))
        );
        assert_matches!(
            tokens.next(),
            Some(IT::SpecialChar(SC::NamedEntity(Entity {
                characters: "∩",
                ..
            })))
        );
        assert_matches!(tokens.next(), Some(IT::Str(s)) if s == "&unknown;");
    }

    #[test]
    fn test_make_line_rules() {
        use super::StyleKind::*;
        use super::StyleSpecifierStart::*;

        let mut tokens = make_line_rules(
            "This is a test. SIZE(16){This is a large text.} COLOR(red){This is a red text.}",
        );
        assert_matches!(tokens.next(), Some(IT::Str(s)) if s == "This is a test. ");
        assert_matches!(tokens.next(), Some(IT::StyleSpecifierStart(SizeBlock)));
        assert_matches!(tokens.next(), Some(IT::Str(s)) if s == "16");
        assert_matches!(tokens.next(), Some(IT::StyleStart(Span)));
        assert_matches!(tokens.next(), Some(IT::Str(s)) if s == "This is a large text.");
        assert_matches!(tokens.next(), Some(IT::StyleEnd(Span)));
        assert_matches!(tokens.next(), Some(IT::Str(s)) if s == " ");
        assert_matches!(tokens.next(), Some(IT::StyleSpecifierStart(ColorBlock)));
        assert_matches!(tokens.next(), Some(IT::Str(s)) if s == "red");
        assert_matches!(tokens.next(), Some(IT::StyleStart(Span)));
        assert_matches!(tokens.next(), Some(IT::Str(s)) if s == "This is a red text.");
        assert_matches!(tokens.next(), Some(IT::StyleEnd(Span)));
    }
}
