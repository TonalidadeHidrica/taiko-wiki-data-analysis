use lazy_static::lazy_static;

use self::super::structs::Document;
use crate::pukiwiki_reparser::structs::{
    list, table, BlockElement, BlockElements, HeadingType, InlineElement, InlineElements, LinkType,
    Unknown,
};
use ego_tree::NodeRef;
use itertools::Itertools;
use scraper::{node, ElementRef, Html, Node, Selector};
use std::convert::identity;

impl Document {
    pub fn parse(document: &Html) -> Document {
        lazy_static! {
            static ref BODY_SELECTOR: Selector = Selector::parse("body").unwrap();
        }

        Document(BlockElements::parse(
            document.select(&*BODY_SELECTOR).next().unwrap(),
        ))
    }
}

impl BlockElements {
    fn parse(parent: ElementRef) -> BlockElements {
        BlockElements(parse_block_elements(parent))
    }
}

fn parse_block_elements(parent: ElementRef) -> Vec<BlockElement> {
    let mut elements = Vec::new();
    for child in parent.children() {
        match (ElementRef::wrap(child), child.value()) {
            (Some(element), _) => elements
                .push(parse_block_element_from_element_ref(element).unwrap_or_else(identity)),
            (_, Node::Text(text)) => {
                if !text.chars().all(|x| x == '\n') {
                    panic!("Unexpected text: {:?}", text);
                }
            }
            (_, node) => elements.push(BlockElement::Unknown(Unknown::Node(node.to_owned()))),
        }
    }
    elements
}

fn parse_block_element_from_element_ref(element: ElementRef) -> Result<BlockElement, BlockElement> {
    lazy_static! {
        static ref TABLE_WRAPPER_SELECTOR: Selector =
            Selector::parse(r#"div[class="ie5"]"#).unwrap();
        static ref TABLE_SELECTOR: Selector = Selector::parse("table").unwrap();
        static ref HEADING_SELECTOR: Selector = Selector::parse("h2,h3,h4").unwrap();
        static ref LIST_SELECTOR: Selector = Selector::parse("ul,ol").unwrap();
    }

    if (*TABLE_WRAPPER_SELECTOR).matches(&element) {
        let table = element
            .children()
            .exactly_one()
            .map(ElementRef::wrap)
            .unwrap()
            .unwrap();
        assert!((*TABLE_SELECTOR).matches(&table));
        Ok(BlockElement::Table(parse_table(table)))
    } else if (*HEADING_SELECTOR).matches(&element) {
        Ok(BlockElement::Heading {
            level: match element.value().name() {
                "h2" => HeadingType::H2,
                "h3" => HeadingType::H3,
                "h4" => HeadingType::H4,
                _ => panic!(),
            },
            contents: InlineElements(parse_inline_elements(element)),
        })
    } else if (*LIST_SELECTOR).matches(&element) {
        Ok(BlockElement::List(parse_list(element)))
    } else {
        Err(BlockElement::Unknown(Unknown::from(&element)))
    }
}

impl InlineElements {
    fn parse(parent: ElementRef) -> InlineElements {
        InlineElements(parse_inline_elements(parent))
    }
}

fn parse_inline_elements(parent: ElementRef) -> Vec<InlineElement> {
    parent
        .children()
        .flat_map(parse_inline_element)
        .collect_vec()
}

fn parse_inline_element(node: NodeRef<Node>) -> Vec<InlineElement> {
    let mut elements = Vec::new();
    match (ElementRef::wrap(node), node.value()) {
        (Some(element), _) => {
            elements.push(parse_inline_element_from_element_ref(element).unwrap_or_else(identity))
        }
        (None, Node::Text(node::Text { text })) => {
            elements.push(InlineElement::Text(text.to_string()));
        }
        (_, node) => elements.push(InlineElement::Unknown(Unknown::Node(node.to_owned()))),
    }
    elements
}

fn parse_inline_element_from_element_ref(
    element: ElementRef,
) -> Result<InlineElement, InlineElement> {
    lazy_static! {
        static ref BR_SELECTOR: Selector = Selector::parse("br.spacer").unwrap();
        static ref ANCHOR_SELECTOR: Selector = Selector::parse("a.anchor[id]").unwrap();
        static ref INTERNAL_LINK_SELECTOR: Selector = Selector::parse("a[title]").unwrap();
    }

    if element.value().name() == "strong" {
        Ok(InlineElement::Strong(InlineElements::parse(element)))
    } else if (*BR_SELECTOR).matches(&element) {
        Ok(InlineElement::Br)
    } else if (*ANCHOR_SELECTOR).matches(&element) {
        assert!(element.children().next().is_none());
        Ok(InlineElement::Anchor(
            element.value().attr("id").unwrap().to_string(),
        ))
    } else if (*INTERNAL_LINK_SELECTOR).matches(&element) {
        Ok(InlineElement::Link {
            href: LinkType::WikiPage(element.value().attr("title").unwrap().to_string()),
            contents: InlineElements::parse(element),
        })
    } else {
        Err(InlineElement::Unknown(Unknown::from(&element)))
    }
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

fn parse_list(parent: ElementRef) -> list::List {
    let kind = match parent.value().name() {
        "ul" => list::Kind::UNORDERED,
        "ol" => list::Kind::ORDERED,
        _ => panic!(),
    };

    let items = parent
        .children()
        .filter_map(|child| match (child.value(), ElementRef::wrap(child)) {
            (Node::Text(text), _) => {
                assert!(text.to_string() == "\n");
                None
            }
            (_, Some(element)) => {
                assert_eq!(element.value().name(), "li");
                let children = element.children().flat_map(parse_list_item).collect_vec();
                Some(list::Item(children))
            }
            _ => panic!(),
        })
        .collect_vec();

    list::List { kind, items }
}

fn parse_list_item(element: NodeRef<Node>) -> Option<list::Element> {
    match element.value() {
        Node::Text(text) => Some(list::Element::Inline(InlineElement::Text(text.to_string()))),
        Node::Element(_) => {
            let element = ElementRef::wrap(element).unwrap();
            Some(
                parse_block_element_from_element_ref(element)
                    .map(list::Element::Block)
                    .unwrap_or_else(|_| {
                        list::Element::Inline(
                            parse_inline_element_from_element_ref(element).unwrap_or_else(identity),
                        )
                    }),
            )
        }
        _ => panic!(),
    }
}

impl From<&'_ ElementRef<'_>> for Unknown {
    fn from(element: &ElementRef) -> Self {
        // This always returns Unknown::Element
        Self::Element {
            text: element.text().collect(),
            html: element.html(),
        }
    }
}

#[test]
fn it_parses_inline_text() {
    let fragment = Html::parse_fragment("hoge");
    let parsed = parse_inline_elements(fragment.root_element());
    assert_eq!(parsed, vec![InlineElement::Text("hoge".to_string())]);
}

//noinspection DuplicatedCode
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
        },
    );
}

//noinspection DuplicatedCode
#[test]
fn it_parses_block_elements_of_table() {
    let html = Html::parse_fragment(
        r#"<div><div class="ie5"><table><tr><td>hoge</td></tr></table></div></div>"#,
    );
    let parsed =
        parse_block_elements(ElementRef::wrap(html.root_element().first_child().unwrap()).unwrap());
    assert_eq!(
        parsed,
        vec![BlockElement::Table(table::Table {
            headers: vec![],
            body: vec![table::Row(vec![table::Cell {
                row_span: 1,
                col_span: 1,
                contents: InlineElements(vec![InlineElement::Text("hoge".to_string())]),
            }])],
        },),],
    );
}
