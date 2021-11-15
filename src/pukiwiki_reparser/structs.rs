use scraper::node::Node;

#[derive(Debug)]
pub struct Document(pub BlockElements);

#[derive(Debug)]
pub struct BlockElements(pub Vec<BlockElement>);

#[derive(Debug, PartialEq)]
pub enum BlockElement {
    Table(table::Table),
    Heading {
        level: HeadingType,
        contents: InlineElements,
    },
    List(list::List),
    Unknown(Unknown),
}

#[derive(Debug, PartialEq)]
pub enum HeadingType {
    H2,
    H3,
    H4,
}

pub mod table {
    use super::InlineElements;

    #[derive(Debug, PartialEq)]
    pub struct Table {
        pub headers: Vec<Row>,
        pub body: Vec<Row>,
    }

    #[derive(Debug, PartialEq)]
    pub struct Row(pub Vec<Cell>);

    #[derive(Debug, PartialEq)]
    pub struct Cell {
        pub row_span: u32,
        pub col_span: u32,
        pub contents: InlineElements,
    }
}

pub mod list {
    use super::{BlockElement, InlineElement};

    #[derive(Debug, PartialEq)]
    pub struct List {
        pub kind: Kind,
        pub items: Vec<Item>,
    }

    #[derive(Debug, PartialEq)]
    pub enum Kind {
        ORDERED,
        UNORDERED,
    }

    #[derive(Debug, PartialEq)]
    pub struct Item(pub Vec<Element>);

    #[derive(Debug, PartialEq)]
    pub enum Element {
        Block(BlockElement),
        Inline(InlineElement),
    }
}

#[derive(Debug, PartialEq)]
pub struct InlineElements(pub Vec<InlineElement>);

#[derive(Debug, PartialEq)]
pub enum InlineElement {
    Text(String),
    Strong(InlineElements),
    Br,
    Link {
        href: LinkType,
        contents: InlineElements,
    },
    Anchor(String),
    Unknown(Unknown),
}

#[derive(Debug, PartialEq)]
pub enum LinkType {
    Target(String),
    WikiPage(String),
    External(String),
}

#[derive(Debug, PartialEq)]
pub enum Unknown {
    Element { text: String, html: String },
    Node(Node),
}
