use std::{borrow::Cow, iter::empty};

use auto_enums::auto_enum;
use getset::{CopyGetters, Getters};
use itertools::Itertools;
use len_trait::{Empty, Len};

use crate::{
    pcre, regex,
    regex_ext::iter::{IndexOwned, MatchComponent, MatchIterator},
};

use super::{
    config::ParserConfig,
    str_ext::{TwoStr, TwoStrConcatRef},
    token::{make_line_rules, InlineToken},
};

#[derive(Debug, derive_more::From)]
pub enum InlineElement<'a> {
    InlineToken(InlineToken<'a>),
    InlinePlugin(InlinePlugin<'a>),
    Footnote(Footnote<'a>),
    Link(Link<'a>),
    InterWikiUrl(InterWikiUrl<'a>),
    MailTo(MailTo<'a>),
    Image(Image<'a>),
    InterWikiNameUrl(InterWikiNameUrl<'a>),
    PageLink(PageLink<'a>),
}
#[derive(Debug, Getters)]
#[getset(get = "pub")]
pub struct InlinePlugin<'a> {
    plugin_name: TwoStr<'a>,
    parameter: Option<TwoStr<'a>>,
    body: Option<Vec<InlineElement<'a>>>,
}
#[derive(Debug, Getters)]
#[getset(get = "pub")]
pub struct Footnote<'a> {
    contents: Vec<InlineElement<'a>>,
}
#[derive(Debug, Getters)]
#[getset(get = "pub")]
pub struct Link<'a> {
    url: TwoStr<'a>,
    caption: Vec<InlineElement<'a>>,
}
#[derive(Debug, Getters)]
#[getset(get = "pub")]
pub struct InterWikiUrl<'a> {
    url: TwoStr<'a>,
    caption: Vec<InlineElement<'a>>,
}
#[derive(Debug, Getters)]
#[getset(get = "pub")]
pub struct MailTo<'a> {
    address: TwoStr<'a>,
    caption: Vec<InlineElement<'a>>,
}
#[derive(Debug, Getters)]
#[getset(get = "pub")]
pub struct Image<'a> {
    src: TwoStr<'a>,
    alt: MaybeConcat<TwoStr<'a>>,
}
#[derive(Debug, Getters)]
#[getset(get = "pub")]
pub struct InterWikiNameUrl<'a> {
    destination: InterWikiDestination<'a>,
    anchor: Option<TwoStr<'a>>,
    name: TwoStr<'a>,
    caption: Vec<InlineElement<'a>>,
}
#[derive(Debug)]
pub enum InterWikiDestination<'a> {
    Myself {
        page_name: TwoStr<'a>,
    },
    Other {
        base_url: Option<TwoStr<'a>>, // If not exists, it indicates the wiki itself
        param: TwoStr<'a>,
        opt: TwoStr<'a>,
    },
}

#[derive(Clone, Copy, Debug, derive_more::From)]
pub enum MaybeConcat<T> {
    Single(T),
    /// `self.0` `self.1` `self.2`
    Concat(T, &'static str, T),
}
impl<T> MaybeConcat<T> {
    fn into_mapped<U: From<T>>(self: MaybeConcat<T>) -> MaybeConcat<U> {
        match self {
            Self::Single(a) => MaybeConcat::Single(a.into()),
            Self::Concat(a, s, b) => MaybeConcat::Concat(a.into(), s, b.into()),
        }
    }
}
impl<T: Empty> Empty for MaybeConcat<T> {
    fn is_empty(&self) -> bool {
        match self {
            Self::Single(a) => a.is_empty(),
            Self::Concat(a, s, b) => a.is_empty() && s.is_empty() && b.is_empty(),
        }
    }
}

impl<'a> From<&'a str> for InlineElement<'a> {
    fn from(str: &'a str) -> Self {
        InlineElement::InlineToken(InlineToken::Str(str))
    }
}

pub(super) fn make_link<'a, 'o>(
    text: TwoStrConcatRef<'a, 'o>,
    config: &ParserConfig,
) -> Vec<InlineElement<'o>> {
    pcre!(
        r###"
            ( # Link_plugin (1)
                &
                (      # (1) plain
                 (\w+) # (2) plugin name
                 (?:
                  \(
                   ((?:(?!\)[;{]).)*) # (3) parameter
                  \)
                 )?
                )
                (?:
                 \{
                  ((?:(?R)|(?!};).)*) # (4) body
                 \}
                )?
                ;
            )
        |
            ( # Link_note (6)
                \(\(
                 ((?:(?R)|(?!\)\)).)*) # (1) note body
                \)\)
            )
        |
            ( # Link_url (8)
                (\[\[             # (1) open bracket
                 ((?:(?!\]\]).)+) # (2) alias
                 (?:>|:)
                )?
                (                 # (3) url
                 (?:(?:https?|ftp|news):\/\/|mailto:)[\w\/\@$()!?&%#:;.,~'=*+-]+
                )
                (?(9)\]\])      # close bracket
            )
        |
            ( # Link_url_interwiki (12)
                \[       # open bracket
                (        # (1) url
                 (?:(?:https?|ftp|news):\/\/|\.\.?\/)[!~*'();\/?:\@&=+$,%#\w.-]*
                )
                \s
                ([^\]]+) # (2) alias
                \]       # close bracket
            )
        |
            ( # Link_mailto (15)
                (?:
                 \[\[
                 ((?:(?!\]\]).)+)(?:>|:)  # (1) alias
                )?
                ([\w.-]+@[\w-]+\.[\w.-]+) # (2) mailto
                (?(16)\]\])              # close bracket if (1)
            )
        |
            ( # Link_interwikiname (18)
                \[\[                  # open bracket
                (?:
                 ((?:(?!\]\]).)+)>    # (1) alias
                )?
                (\[\[)?               # (2) open bracket
                ((?:(?!\s|:|\]\]).)+) # (3) InterWiki
                (?<! > | >\[\[ )      # not '>' or '>[['
                :                     # separator
                (                     # (4) param
                 (\[\[)?              # (5) open bracket
                 (?:(?!>|\]\]).)+
                 (?(23)\]\])         # close bracket if (5)
                )
                (?(20)\]\])          # close bracket if (2)
                \]\]                  # close bracket
            )
        |
            ( # Link_bracketname (24)
                \[\[                     # Open bracket
                (?:((?:(?!\]\]).)+)>)?   # (1) Alias
                (\[\[)?                  # (2) Open bracket
                (                        # (3) PageName
                 (?:(?:[A-Z][a-z]+){2,}(?!\w))
                 |
                 (?:(?!\s):?[^\r\n\t\f\[\]<>#&":]+:?(?<!\s))
                )?
                (\#(?:[a-zA-Z][\w-]*)?)? # (4) Anchor
                (?(26)\]\])             # Close bracket if (2)
                \]\]                     # Close bracket
            )
        |
            ( # Link_wikiname (29)
                ((?:[A-Z][a-z]+){2,}(?!\w))
            )
        "###,
        x
    )
    .with(|pattern| {
        let mut res: Vec<InlineElement> = Vec::new();
        for groups in text
            .pcre_matches(pattern)
            .match_components(text, text.len())
        {
            let groups = match groups {
                MatchComponent::Match(m) => m,
                MatchComponent::Between(str) => {
                    res.extend(make_line_rules(str).map(InlineElement::InlineToken));
                    continue;
                }
            };
            if let Some(group) = groups.group_opt(1) {
                // Link_plugin
                let plugin_name = groups.group(3).into();
                let parameter = groups.group_opt(4).map(Into::into);
                let body = groups.group_opt(5).map(|s| make_link(s, config));
                if exist_plugin_inline(plugin_name) {
                    let parsed = InlinePlugin {
                        plugin_name,
                        parameter,
                        body,
                    };
                    res.push(parsed.into());
                } else {
                    res.extend(make_line_rules(group).map(InlineElement::InlineToken));
                }
            } else if groups.group_opt(6).is_some() {
                // Link_note
                let body = groups.group(7);
                let contents = make_link(body, config);
                res.push(Footnote { contents }.into());
            } else if groups.group_opt(8).is_some() {
                // Link_url
                let alias = groups.group_opt(10);
                let url = groups.group(11);
                let caption = alias.map_or_else(Vec::new, |alias| parse_alias(url, alias, config));
                res.push(
                    Link {
                        caption,
                        url: url.into(),
                    }
                    .into(),
                );
            } else if groups.group_opt(12).is_some() {
                // Link_url_interwiki
                let url = groups.group(13);
                let alias = groups.group(14);
                let caption = parse_alias(url, alias, config);
                res.push(
                    InterWikiUrl {
                        url: url.into(),
                        caption,
                    }
                    .into(),
                );
            } else if groups.group_opt(15).is_some() {
                // Link_mailto
                let alias = groups.group_opt(16);
                let mailto = groups.group(17);
                let caption =
                    alias.map_or_else(Vec::new, |alias| parse_alias(mailto, alias, config));
                let parsed = MailTo {
                    caption,
                    address: mailto.into(),
                };
                res.push(parsed.into());
            } else if groups.group_opt(18).is_some() {
                // Link_interwikiname
                let alias = groups.group_opt(19);
                let name = groups.group_obj(21).unwrap();
                let param = groups.group_obj(22).unwrap();
                let (param, param_end, anchor) = if let Some(captures) = param
                    .as_str()
                    .regex_captures(regex!(r"^([^#]+)(#[A-Za-z][\w-]*)$"))
                {
                    let before_anchor = captures.get(1).unwrap();
                    (
                        before_anchor.as_str(),
                        param.start() + before_anchor.end(),
                        captures.get(2).map(|x| x.as_str()),
                    )
                } else {
                    (param.as_str(), param.end(), None)
                };
                let caption = alias.map_or_else(Vec::new, |alias| {
                    // name (18+3) and param(18+4) are adjacent, separated by ":"
                    // (even if the alias was split off)
                    let name_and_alias = text.index_owned(name.start()..param_end);
                    parse_alias(name_and_alias, alias, config)
                });
                let name = name.as_str();
                let destination =
                    get_interwiki_url(name, param).unwrap_or(InterWikiDestination::Myself {
                        page_name: param.into(),
                    });
                let parsed = InterWikiNameUrl {
                    destination,
                    anchor: anchor.map(Into::into),
                    name: name.into(),
                    caption,
                };
                res.push(parsed.into());
            } else if let Some(group) = groups.group_obj(24) {
                // Link_bracketname
                let alias = groups.group_obj(25);
                let name = groups.group_obj(27);
                let anchor = groups.group_obj(28);

                let alias_str = alias.as_ref().map(|x| x.as_str());
                let name_str = name.as_ref().map(|x| x.as_str());
                let anchor_str = anchor.as_ref().map(|x| x.as_str());

                let name_str_some = name_str.unwrap_or_default();

                let name_is_empty = name.as_ref().map_or(true, |s| s.as_str().is_empty());
                if name_is_empty && anchor.as_ref().map_or(true, |s| s.as_str().is_empty()) {
                    res.extend(make_line_rules(group.as_str()).map(InlineElement::InlineToken));
                } else if name_is_empty
                    // not wikiname
                    || pcre!(r"^(?:[A-Z][a-z]+){2,}(?!\w)$" => (name_str_some).pcre_exec).is_none()
                {
                    // TODO [compl] if the page does not exist, it should return plain text instead
                    // TODO [compl] update name with absolute name
                    let alias = if alias.as_ref().map_or(true, |x| x.as_str().is_empty()) {
                        if let Some(anchor) = anchor {
                            let start = name.as_ref().map_or(anchor.start(), |x| x.end());
                            // Some((name.unwrap_or_default().into(), "", anchor).into())
                            Some(text.index_owned(start..anchor.end()))
                        } else {
                            name_str
                        }
                    } else {
                        alias_str
                    };
                    let contents = alias
                        .map_or_else(Vec::new, |alias| parse_alias(name_str_some, alias, config));
                    res.push(
                        make_page_link(
                            name_str.map(|x| x.into()),
                            contents,
                            anchor_str.map(|x| x.into()),
                            false,
                        )
                        .into(),
                    );
                }
            } else if groups.group_opt(29).is_some() {
                // Link_wikiname
                let wikiname = groups.group(30).into();
                res.push(
                    make_page_link(
                        Some(wikiname),
                        wikiname.into_iter().map(|x| x.into()).collect_vec(),
                        None,
                        false,
                    )
                    .into(),
                );
            } else {
                unreachable!("The regex has either of these groups")
            };
        }
        res
    })
}
fn exist_plugin_inline<'o>(_plugin_name: impl Into<TwoStr<'o>>) -> bool {
    true
}
#[derive(Clone, Copy, derive_more::From)]
enum MaybeImgUrl<T> {
    Maybe(T),
    /// Used for Link_braketname where it ends with `#(?:[a-zA-Z][\w-]*)?`
    Not(MaybeConcat<T>),
}
impl<'a, 'o> MaybeImgUrl<TwoStrConcatRef<'a, 'o>> {
    fn as_img_url(self) -> Option<TwoStr<'o>> {
        match self {
            Self::Maybe(alias) => {
                let is_img = is_url(alias) && alias.regex_is_match(regex!(r"\.(gif|png|jpe?g)$"));
                is_img.then(|| alias.into())
            }
            Self::Not(_) => None,
        }
    }
}
impl<T> From<MaybeImgUrl<T>> for MaybeConcat<T> {
    fn from(this: MaybeImgUrl<T>) -> Self {
        match this {
            MaybeImgUrl::Maybe(a) => a.into(),
            MaybeImgUrl::Not(a) => a,
        }
    }
}
fn parse_alias<'o, 'a>(
    name: impl Into<MaybeConcat<TwoStrConcatRef<'o, 'a>>>,
    alias: TwoStrConcatRef<'o, 'a>,
    config: &ParserConfig,
) -> Vec<InlineElement<'a>> {
    let name = name.into();
    if config.disable_inline_image_from_uri {
        if let Some(src) = MaybeImgUrl::from(alias).as_img_url() {
            let parsed = Image {
                src,
                alt: name.into_mapped(),
            };
            return vec![parsed.into()];
        }
    }
    if !alias.is_empty() {
        // $page is an external information, so we don't
        // TODO make_link converts plugin, but here it shouldn't
        // TODO make_line_rules
        make_link(alias, config)
    } else {
        vec![]
    }
}
fn is_url<'a, 'o>(str: impl Into<TwoStrConcatRef<'a, 'o>>) -> bool {
    // assume $only_http = FALSE
    // Unnecessary escape sequences are removed from the regex
    str.into().regex_is_match(regex!(
        r"^(https?|ftp|news)(://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]*)$"
    ))
}
fn get_interwiki_url<'a, 'o>(
    _name: TwoStrConcatRef<'a, 'o>,
    _param: TwoStrConcatRef<'a, 'o>,
) -> Option<InterWikiDestination<'o>> {
    // TODO: [prio:low] implement
    // Note that taiko wiki does not have contents in InterWikiName,
    // so this does not affect to the parse result.
    None
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use crate::pukiwiki_parser::config::ParserConfig;

    use super::{
        is_url, make_link, InlineElement as IE, InlinePlugin, InlineToken as IT, InterWikiUrl,
        Link, MailTo, PageLink,
    };

    #[test]
    fn test_is_url() {
        assert!(is_url("https://example.com/example.png"));
        assert!(!is_url("https://example.com/example.png "));
        assert!(!is_url("   https://example.com/example.png"));
        assert!(!is_url("this_is_not_url"));
    }

    #[test]
    fn test_make_link() {
        let config = ParserConfig::default();

        // Inline plugin
        let res = make_link("&ref(image.png,nolink);".into(), &config);
        assert_eq!(res.len(), 1);
        assert_matches!(
            &res[0],
            IE::InlinePlugin(InlinePlugin { plugin_name, parameter: Some(param), body: None })
            if plugin_name == "ref" && param == "image.png,nolink"
        );

        // Footnote
        let res = make_link("text((footnote))".into(), &config);
        assert_eq!(res.len(), 2);
        assert_matches!(res[0], IE::InlineToken(IT::Str(s)) if s == "text");
        let elems = assert_matches!(&res[1], IE::Footnote(footnote) => &footnote.contents);
        assert_eq!(elems.len(), 1);
        assert_matches!(elems[0], IE::InlineToken(IT::Str(s)) if s == "footnote");

        // Link with brackets, separated by :
        let res = make_link("[[link:https://example.com]]".into(), &config);
        assert_eq!(res.len(), 1);
        let caption = assert_matches!(
            &res[0],
            IE::Link(Link { caption, url }) if url == "https://example.com"
            => caption
        );
        assert_eq!(caption.len(), 1);
        assert_matches!(caption[0], IE::InlineToken(IT::Str(s)) if s == "link");

        // Link with brackets, separated by `>`
        let res = make_link("[[link>https://example.com]]".into(), &config);
        assert_eq!(res.len(), 1);
        let caption = assert_matches!(
            &res[0],
            IE::Link(Link { caption, url }) if url == "https://example.com"
            => caption
        );
        assert_eq!(caption.len(), 1);
        assert_matches!(caption[0], IE::InlineToken(IT::Str(s)) if s == "link");

        // Link without brackets
        let res = make_link("https://example.com".into(), &config);
        assert_eq!(res.len(), 1);
        assert_matches!(
            &res[0],
            IE::Link(Link { caption, url }) if url == "https://example.com" && caption.is_empty()
        );

        // Interwiki URL definition?
        let res = make_link(
            "[https://pukiwiki.osdn.jp/index.php pukiwiki]".into(),
            &config,
        );
        assert_eq!(res.len(), 1);
        let caption = assert_matches!(
            &res[0],
            IE::InterWikiUrl(InterWikiUrl { caption, url })
            if url == "https://pukiwiki.osdn.jp/index.php"
            => caption
        );
        assert_eq!(caption.len(), 1);
        assert_matches!(caption[0], IE::InlineToken(IT::Str(s)) if s == "pukiwiki");

        // Mailto with brackets
        let res = make_link("[[alias>example@example.com]]".into(), &config);
        assert_eq!(res.len(), 1);
        let caption = assert_matches!(
            &res[0],
            IE::MailTo(MailTo { address, caption })
            if address == "example@example.com"
            => caption
        );
        assert_eq!(caption.len(), 1);
        assert_matches!(caption[0], IE::InlineToken(IT::Str(s)) if s == "alias");

        // Wiki link
        let res = make_link("[[リンク>ページ名#anchor]]".into(), &config);
        let caption = assert_matches!(
            &res[..],
            [IE::PageLink(PageLink {
                page: Some(page),
                contents,
                anchor: Some(anchor),
                is_auto_link: false
            })]
            if page == "ページ名" && anchor == "#anchor"
            => contents
        );
        assert_matches!(caption[..], [IE::InlineToken(IT::Str(s))] if s == "リンク");

        // Wikiname
        let res = make_link("WikiName".into(), &config);
        let contents = assert_matches!(
            &res[..],
            [IE::PageLink(PageLink {page: Some(page), contents, anchor: None, is_auto_link: false})]
            if page == "WikiName"
            => contents
        );
        assert_matches!(contents[..], [IE::InlineToken(IT::Str(s))] if s == "WikiName");
    }
}

#[derive(Debug, Getters, CopyGetters)]
pub struct PageLink<'a> {
    #[getset(get = "pub")]
    page: Option<TwoStr<'a>>,
    #[getset(get = "pub")]
    contents: Vec<InlineElement<'a>>,
    #[getset(get = "pub")]
    anchor: Option<TwoStr<'a>>,
    #[getset(get_copy = "pub")]
    is_auto_link: bool,
}
fn make_page_link<'a>(
    page: impl Into<Option<TwoStr<'a>>>,
    contents: Vec<InlineElement<'a>>,
    anchor: Option<TwoStr<'a>>,
    is_auto_link: bool,
) -> PageLink<'a> {
    PageLink {
        page: page.into(),
        contents,
        anchor,
        is_auto_link,
    }
}

// Itertors
impl InlineElement<'_> {
    #[auto_enum(Iterator)]
    pub fn text(&self) -> impl Iterator<Item = Cow<str>> {
        match self {
            InlineElement::InlineToken(x) => x.text(),
            InlineElement::PageLink(x) => {
                Box::new(x.contents().iter().flat_map(|x| x.text())) as Box<dyn Iterator<Item = _>>
            }
            _ => empty(),
            // InlineElement::InlinePlugin(_) => todo!(),
            // InlineElement::Footnote(_) => todo!(),
            // InlineElement::Link(_) => todo!(),
            // InlineElement::InterWikiUrl(_) => todo!(),
            // InlineElement::MailTo(_) => todo!(),
            // InlineElement::Image(_) => todo!(),
            // InlineElement::InterWikiNameUrl(_) => todo!(),
        }
    }
}
