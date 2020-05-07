use scraper::node::Node;

#[derive(Debug)]
pub struct Document(pub BodyElements);

#[derive(Debug)]
pub struct BodyElements(pub Vec<BodyElement>);

#[derive(Debug)]
pub enum BodyElement {
    Table(table::Table),
    Unknown(Node),
}

pub mod table {
    use self::super::InlineElements;

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
    UnknownElement {
        text: String,
        html: String,
    },
    UnknownNode(Node),
}

#[derive(Debug, PartialEq)]
pub enum LinkType {
    Target(String),
    WikiPage(String),
    External(String),
}
