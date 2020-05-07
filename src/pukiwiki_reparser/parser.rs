use lazy_static::lazy_static;

use self::super::structs::Document;
use crate::pukiwiki_reparser::structs::{
    table, BodyElement, BodyElements, InlineElement, InlineElements, LinkType,
};
use itertools::Itertools;
use scraper::{node, ElementRef, Html, Node, Selector};

impl Document {
    pub fn parse(document: &Html) -> Document {
        lazy_static! {
            static ref BODY_SELECTOR: Selector = Selector::parse("body").unwrap();
        }

        Document(BodyElements::parse(
            document.select(&*BODY_SELECTOR).next().unwrap(),
        ))
    }
}

impl BodyElements {
    fn parse(parent: ElementRef) -> BodyElements {
        BodyElements(parse_body_elements(parent))
    }
}

fn parse_body_elements(parent: ElementRef) -> Vec<BodyElement> {
    let mut elements = Vec::new();
    for child in parent.children() {
        match child.value() {
            Node::Element(element) if element.name() == "div" => {
                if !(element.classes.len() == 1
                    && element.classes.iter().collect_vec() == vec!["ie5"])
                {
                    continue;
                }
                let mut children = child.children().map(ElementRef::wrap);
                if let (Some(Some(element)), None) = (children.next(), children.next()) {
                    if element.value().name() != "table" {
                        continue;
                    }
                    elements.push(BodyElement::Table(parse_table(element)));
                }
            }
            node => elements.push(BodyElement::Unknown(node.to_owned())),
        }
    }
    elements
}

impl InlineElements {
    fn parse(parent: ElementRef) -> InlineElements {
        InlineElements(parse_inline_elements(parent))
    }
}

fn parse_inline_elements(parent: ElementRef) -> Vec<InlineElement> {
    lazy_static! {
        static ref BR_SELECTOR: Selector = Selector::parse("br.spacer").unwrap();
        static ref ANCHOR_SELECTOR: Selector = Selector::parse("a.anchor[id]").unwrap();
        static ref INTERNAL_LINK_SELECTOR: Selector = Selector::parse("a[title]").unwrap();
    }

    let mut elements = Vec::new();
    for child in parent.children() {
        match (ElementRef::wrap(child), child.value()) {
            (Some(element), _) => {
                if element.value().name() == "strong" {
                    elements.push(InlineElement::Strong(InlineElements::parse(element)));
                } else if (*BR_SELECTOR).matches(&element) {
                    elements.push(InlineElement::Br);
                } else if (*ANCHOR_SELECTOR).matches(&element) {
                    assert!(child.children().next().is_none());
                    elements.push(InlineElement::Anchor(
                        element.value().attr("id").unwrap().to_string(),
                    ));
                } else if (*INTERNAL_LINK_SELECTOR).matches(&element) {
                    elements.push(InlineElement::Link {
                        href: LinkType::WikiPage(
                            element.value().attr("title").unwrap().to_string(),
                        ),
                        contents: InlineElements::parse(element),
                    });
                } else {
                    elements.push(InlineElement::UnknownElement {
                        text: element.text().collect(),
                        html: element.html(),
                    });
                }
            }
            (None, Node::Text(node::Text { text })) => {
                elements.push(InlineElement::Text(text.to_string()));
            }
            (_, node) => elements.push(InlineElement::UnknownNode(node.to_owned())),
        }
    }
    elements
}

fn parse_table(parent: ElementRef) -> table::Table {
    lazy_static! {
        static ref TBODY_TR_SELECTOR: Selector = Selector::parse(":scope > tbody > tr").unwrap();
        static ref TD_SELECTOR: Selector = Selector::parse(":scope > td").unwrap();
    }

    let body = parent
        .select(&*TBODY_TR_SELECTOR)
        .map(|tr| {
            table::Row(
                tr.select(&*TD_SELECTOR)
                    .map(|td| {
                        let get_int = |attr: &'static str| {
                            td.value()
                                .attr(attr)
                                .and_then(|s| s.parse::<u32>().ok())
                                .unwrap_or(1)
                        };
                        table::Cell {
                            row_span: get_int("rowspan"),
                            col_span: get_int("colspan"),
                            contents: InlineElements::parse(td),
                        }
                    })
                    .collect_vec(),
            )
        })
        .collect_vec();
    table::Table {
        headers: Vec::new(),
        body,
    }
}

#[test]
fn it_parses_inline_text() {
    let fragment = Html::parse_fragment("hoge");
    let parsed = parse_inline_elements(fragment.root_element());
    assert_eq!(parsed, vec![InlineElement::Text("hoge".to_string())]);
}

#[test]
fn it_parses_table() {
    let table = Html::parse_fragment("<table><tr><td>hoge</td></tr></table>");
    let parsed =
        parse_table(ElementRef::wrap(table.root_element().first_child().unwrap()).unwrap());
    assert_eq!(
        parsed,
        table::Table {
            headers: vec![],
            body: vec![table::Row(vec![table::Cell {
                row_span: 1,
                col_span: 1,
                contents: InlineElements(vec![InlineElement::Text("hoge".to_string())]),
            }])],
        }
    );
}
