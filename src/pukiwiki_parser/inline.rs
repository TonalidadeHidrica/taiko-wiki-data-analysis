use std::borrow::Cow;

use crate::{
    pcre, regex,
    regex_ext::{
        iter::{MatchComponent, MatchIterator},
        pcre::MatchExt,
    },
};

use super::{
    config::Config,
    token::{make_line_rules, InlineToken},
};

#[derive(Debug, derive_more::From)]
pub enum InlineElement {
    InlineToken(InlineToken),
    InlinePlugin(InlinePlugin),
    Footnote(Footnote),
    Link(Link),
    InterWikiUrl(InterWikiUrl),
    MailTo(MailTo),
    Image(Image),
    InterWikiNameUrl(InterWikiNameUrl),
    PageLink(PageLink),
}
#[derive(Debug)]
pub struct InlinePlugin {
    plugin_name: String,
    parameter: Option<String>,
    body: Option<Vec<InlineElement>>,
}
#[derive(Debug)]
pub struct Footnote {
    contents: Vec<InlineElement>,
}
#[derive(Debug)]
pub struct Link {
    url: String,
    caption: Vec<InlineElement>,
}
#[derive(Debug)]
pub struct InterWikiUrl {
    url: String,
    caption: Vec<InlineElement>,
}
#[derive(Debug)]
pub struct MailTo {
    address: String,
    caption: Vec<InlineElement>,
}
#[derive(Debug)]
pub struct Image {
    src: String,
    alt: String,
}
#[derive(Debug)]
pub struct InterWikiNameUrl {
    destination: InterWikiDestination,
    anchor: Option<String>,
    name: String,
    caption: Vec<InlineElement>,
}
#[derive(Debug)]
pub enum InterWikiDestination {
    Myself {
        page_name: String,
    },
    Other {
        base_url: Option<String>, // If not exists, it indicates the wiki itself
        param: String,
        opt: String,
    },
}

// Temporary
impl From<String> for InlineElement {
    fn from(string: String) -> Self {
        InlineElement::InlineToken(InlineToken::Str(string))
    }
}

pub(super) fn make_link(text: Cow<str>, config: &Config) -> Vec<InlineElement> {
    let text = text.into_owned();
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
        for groups in pattern.matches(&text).match_components(&text) {
            let groups = match groups {
                MatchComponent::Match(m) => m,
                MatchComponent::Between(str) => {
                    res.extend(make_line_rules(str).map(InlineElement::InlineToken));
                    continue;
                }
            };
            if let Some(group) = groups.group_opt(1) {
                // Link_plugin
                let plugin_name = groups.group(3).to_owned();
                let parameter = groups.group_opt(4).map(str::to_owned);
                let body = groups.group_opt(5).map(|s| make_link(s.into(), config));
                if exist_plugin_inline(&plugin_name) {
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
                let body = groups.group(7).to_owned();
                let contents = make_link(body.into(), config);
                res.push(Footnote { contents }.into());
            } else if groups.group_opt(8).is_some() {
                // Link_url
                let alias = groups.group_opt(10);
                let url = groups.group(11).to_owned();
                let caption =
                    alias.map_or_else(Vec::new, |alias| parse_alias(url.to_owned(), alias, config));
                res.push(Link { caption, url }.into());
            } else if groups.group_opt(12).is_some() {
                // Link_url_interwiki
                let url = groups.group(13).to_owned();
                let alias = groups.group(14);
                let caption = parse_alias(url.to_owned(), alias, config);
                res.push(InterWikiUrl { url, caption }.into());
            } else if groups.group_opt(15).is_some() {
                // Link_mailto
                let alias = groups.group_opt(16);
                let mailto = groups.group(17).to_owned();
                let caption = alias.map_or_else(Vec::new, |alias| {
                    parse_alias(mailto.to_owned(), alias, config)
                });
                let parsed = MailTo {
                    caption,
                    address: mailto,
                };
                res.push(parsed.into());
            } else if groups.group_opt(18).is_some() {
                // Link_interwikiname
                let alias = groups.group_opt(19);
                let name = groups.group(21).to_owned();
                let param = groups.group(22).to_owned();
                let (param, anchor) = if let Some(captures) =
                    regex!(r"^([^#]+)(#[A-Za-z][\w-]*)$").captures(&param)
                {
                    (
                        captures.get(1).unwrap().as_str().to_owned(),
                        captures.get(2).map(|x| x.as_str()),
                    )
                } else {
                    (param, None)
                };
                let caption = alias.map_or_else(Vec::new, |alias| {
                    parse_alias(name.clone() + ":" + &param, alias, config)
                });
                let destination = get_interwiki_url(&name, &param)
                    .unwrap_or(InterWikiDestination::Myself { page_name: param });
                let parsed = InterWikiNameUrl {
                    destination,
                    anchor: anchor.map(str::to_owned),
                    name,
                    caption,
                };
                res.push(parsed.into());
            } else if let Some(group) = groups.group_opt(24) {
                // Link_bracketname
                let alias = groups.group_opt(25).map(str::to_owned);
                let name = groups.group_opt(27).map(str::to_owned);
                let anchor = groups.group_opt(28).map(str::to_owned);
                let name_ref = name.as_deref().unwrap_or("");
                let name_is_empty = name.as_ref().map_or(true, |s| s.is_empty());
                if name_is_empty && anchor.as_ref().map_or(true, |s| s.is_empty()) {
                    res.extend(make_line_rules(group).map(InlineElement::InlineToken));
                } else if name_is_empty
                    // not wikiname
                    || pcre!(r"^(?:[A-Z][a-z]+){2,}(?!\w)$" => exec(name_ref)).is_none()
                {
                    // TODO [compl] if the page does not exist, it should return plain text instead
                    // TODO [compl] update name with absolute name
                    let alias = if alias.as_ref().map_or(true, |x| x.is_empty()) {
                        if let Some(anchor) = &anchor {
                            Some(name.as_ref().cloned().unwrap_or_default() + anchor)
                        } else {
                            name.clone()
                        }
                    } else {
                        alias
                    };
                    let contents = alias.map_or_else(Vec::new, |alias| {
                        parse_alias(name.as_ref().cloned().unwrap_or_default(), &alias, config)
                    });
                    res.push(make_page_link(name, contents, anchor, false).into());
                }
            } else if groups.group_opt(29).is_some() {
                // Link_wikiname
                let wikiname = groups.group(30).to_owned();
                res.push(
                    make_page_link(wikiname.clone(), vec![wikiname.into()], None, false).into(),
                );
            } else {
                unreachable!("The regex has either of these groups")
            };
        }
        res
    })
}
fn exist_plugin_inline(_plugin_name: &str) -> bool {
    true
}
fn parse_alias(name: String, alias: &str, config: &Config) -> Vec<InlineElement> {
    if config.disable_inline_image_from_uri
        && is_url(alias)
        && regex!(r"\.(gif|png|jpe?g)$").is_match(alias)
    {
        let parsed = Image {
            src: alias.to_owned(),
            alt: name,
        };
        vec![parsed.into()]
    } else if !alias.is_empty() {
        // $page is an external information, so we don't
        // TODO make_link converts plugin, but here it shouldn't
        // TODO make_line_rules
        make_link(alias.into(), config)
    } else {
        vec![]
    }
}
fn is_url(str: &str) -> bool {
    // assume $only_http = FALSE
    // Unnecessary escape sequences are removed from the regex
    regex!(r"^(https?|ftp|news)(://[-_.!~*'()a-zA-Z0-9;/?:@&=+$,%#]*)$").is_match(str)
}
fn get_interwiki_url(_name: &str, _param: &str) -> Option<InterWikiDestination> {
    // TODO: [prio:low] implement
    // Note that taiko wiki does not have contents in InterWikiName,
    // so this does not affect to the parse result.
    None
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;

    use crate::pukiwiki_parser::config::Config;

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
        let config = Config::default();

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
        assert_matches!(&res[0], IE::InlineToken(IT::Str(s)) if s == "text");
        let elems = assert_matches!(&res[1], IE::Footnote(footnote) => &footnote.contents);
        assert_eq!(elems.len(), 1);
        assert_matches!(&elems[0], IE::InlineToken(IT::Str(s)) if s == "footnote");

        // Link with brackets, separated by :
        let res = make_link("[[link:https://example.com]]".into(), &config);
        assert_eq!(res.len(), 1);
        let caption = assert_matches!(
            &res[0],
            IE::Link(Link { caption, url }) if url == "https://example.com"
            => caption
        );
        assert_eq!(caption.len(), 1);
        assert_matches!(&caption[0], IE::InlineToken(IT::Str(s)) if s == "link");

        // Link with brackets, separated by `>`
        let res = make_link("[[link>https://example.com]]".into(), &config);
        assert_eq!(res.len(), 1);
        let caption = assert_matches!(
            &res[0],
            IE::Link(Link { caption, url }) if url == "https://example.com"
            => caption
        );
        assert_eq!(caption.len(), 1);
        assert_matches!(&caption[0], IE::InlineToken(IT::Str(s)) if s == "link");

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
        assert_matches!(&caption[0], IE::InlineToken(IT::Str(s)) if s == "pukiwiki");

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
        assert_matches!(&caption[0], IE::InlineToken(IT::Str(s)) if s == "alias");

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
        assert_matches!(&caption[..], [IE::InlineToken(IT::Str(s))] if s == "リンク");

        // Wikiname
        let res = make_link("WikiName".into(), &config);
        let contents = assert_matches!(
            &res[..],
            [IE::PageLink(PageLink {page: Some(page), contents, anchor: None, is_auto_link: false})]
            if page == "WikiName"
            => contents
        );
        assert_matches!(&contents[..], [IE::InlineToken(IT::Str(s))] if s == "WikiName");
    }
}

#[derive(Debug)]
pub struct PageLink {
    page: Option<String>,
    contents: Vec<InlineElement>,
    anchor: Option<String>,
    is_auto_link: bool,
}
fn make_page_link(
    page: impl Into<Option<String>>,
    contents: Vec<InlineElement>,
    anchor: Option<String>,
    is_auto_link: bool,
) -> PageLink {
    PageLink {
        page: page.into(),
        contents,
        anchor,
        is_auto_link,
    }
}
