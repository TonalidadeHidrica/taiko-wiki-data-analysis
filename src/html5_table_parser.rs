use crate::utils::chmax;
use indexmap::map::IndexMap;
use itertools::Itertools;
use num_traits::clamp;
use std::collections::HashSet;

type TableLikeRet<'a, Group> = Vec<Option<TableChild<&'a Group, &'a <Group as TRGroupLike>::Row>>>;

pub trait TableLike {
    type Group: TRGroupLike;
    // type I: Iterator<Item = Option<TableChild<Self::Group, <Self::Group as TRGroupLike>::Row>>>;
    fn children(&self) -> TableLikeRet<Self::Group>;
}

pub enum TableChild<P, R> {
    ColGroup,
    THead(P),
    TBody(P),
    TFoot(P),
    TR(R),
}

pub trait TRGroupLike {
    type Row: RowLike;
    // type I: Iterator<Item=Option<Self::Row>>;
    fn children(&self) -> Vec<Option<&Self::Row>>;
}

pub trait RowLike {
    type Cell: CellLike;
    // type I: Iterator<Item=Option<Self::Cell>>;
    fn children(&self) -> Vec<Option<&Self::Cell>>;
}

pub trait CellLike {
    type Item;
    fn contents(&self) -> Self::Item;
    fn kind(&self) -> CellKind;
    fn row_span(&self) -> Option<&str>;
    fn col_span(&self) -> Option<&str>;
}

#[derive(Clone, Copy)]
pub enum CellKind {
    TD,
    TH,
}

struct Context<T> {
    x_width: u32,
    x_current: u32,
    y_height: u32,
    y_start: u32,
    y_current: u32,
    list_of_downward_growing_cells: Vec<()>,
    assigned_slots: HashSet<(u32, u32)>,

    cells: Vec<parsed::Cell<T>>,
}

impl<T> Default for Context<T> {
    fn default() -> Self {
        Context {
            x_width: 0,
            x_current: 0,
            y_height: 0,
            y_start: 0,
            y_current: 0,
            list_of_downward_growing_cells: vec![],
            assigned_slots: HashSet::new(),

            cells: vec![],
        }
    }
}

impl<T> Context<T> {
    fn current_is_assigned(&self) -> bool {
        self.assigned_slots
            .contains(&(self.x_current, self.y_current))
    }
}

pub mod parsed {
    use indexmap::map::IndexMap;
    use std::ops::Range;

    #[derive(Debug, PartialEq, Eq)]
    pub struct Table<T> {
        pub rows: IndexMap<u32, Row<T>>,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct Row<T> {
        pub cells: IndexMap<u32, Cell<T>>,
    }

    #[derive(Debug, PartialEq, Eq)]
    pub struct Cell<T> {
        pub contents: T,
        pub row_span: Range<u32>,
        pub col_span: Range<u32>,
    }
}

// https://html.spec.whatwg.org/multipage/tables.html#forming-a-table
pub fn form_table<T>(
    table: T,
) -> parsed::Table<
    <<<<T as TableLike>::Group as TRGroupLike>::Row as RowLike>::Cell as CellLike>::Item,
>
where
    T: TableLike,
{
    let children = table.children();
    let mut children_iter = children.iter().flatten().peekable();

    let mut context = Context::default();
    // 1.
    #[allow(clippy::field_reassign_with_default)]
    {
        context.x_width = 0;
    }
    // 2.
    context.y_height = 0;
    // 3.
    let mut pending_tfoot_elements = Vec::<&T::Group>::new();

    // TODO 4. - 6.

    // 7. 8. -> done in the loop

    // 9.
    while let Some(TableChild::ColGroup) = children_iter.peek() {
        // TODO we will ignore for now
        // 4.
        children_iter.next();
    }

    // 10.
    context.y_current = 0;
    // 11.
    context.list_of_downward_growing_cells = Vec::new();
    // 12.
    for current_element in children_iter {
        match current_element {
            TableChild::ColGroup => continue,
            // 13.
            TableChild::TR(tr) => {
                algorithm_for_processing_rows(&mut context, *tr);
                continue;
            }
            _ => {}
        }
        // 14.
        algorithm_for_ending_a_row_group(&mut context);
        // 15.
        if let TableChild::TFoot(f) = current_element {
            pending_tfoot_elements.push(f);
            continue;
        }

        // 16.
        if let TableChild::THead(parent) | TableChild::TBody(parent) = current_element {
            algorithm_for_processing_row_groups(&mut context, *parent);
        } else {
            unreachable!();
        }
        // 17., 18.
    }
    // 19.
    for foot in pending_tfoot_elements {
        algorithm_for_processing_row_groups(&mut context, foot);
    }

    let mut rows = IndexMap::new();
    for cell in context.cells {
        rows.entry(cell.row_span.start)
            .or_insert_with(|| parsed::Row {
                cells: IndexMap::new(),
            })
            .cells
            .insert(cell.col_span.start, cell);
    }

    parsed::Table { rows }
}

fn algorithm_for_processing_row_groups<P: TRGroupLike>(
    context: &mut Context<<<<P as TRGroupLike>::Row as RowLike>::Cell as CellLike>::Item>,
    parent: &P,
) {
    // 1.
    context.y_start = context.y_height;
    // 2.
    for tr in parent.children().iter().flatten() {
        algorithm_for_processing_rows(context, *tr);
    }
    // 3.
    if context.y_height > context.y_start {
        // TODO: Form a row group; not needed?
    }
    algorithm_for_ending_a_row_group(context);
}

fn algorithm_for_ending_a_row_group<T>(context: &mut Context<T>) {
    // 1.
    while context.y_current > context.y_height {
        // 1.
        algorithm_for_growing_downward_growing_cells(context);
        // 2.
        context.y_current += 1;
    }
    // 2.
    context.list_of_downward_growing_cells.clear();
}

fn algorithm_for_processing_rows<R: RowLike>(
    context: &mut Context<<<R as RowLike>::Cell as CellLike>::Item>,
    tr: &R,
) {
    // 1.
    if context.y_height == context.y_current {
        context.y_height += 1;
        assert!(!(context.y_current > context.y_height));
    }
    // 2.
    context.x_current = 0;
    // 3.
    algorithm_for_growing_downward_growing_cells(context);
    // 4., 5., 17.
    for current_cell in tr.children().iter().flatten() {
        // 6.
        while context.x_current < context.x_width && context.current_is_assigned() {
            context.x_current += 1;
        }
        // 7.
        if context.x_current == context.x_width {
            context.x_width += 1;
            assert!(!(context.x_current > context.x_width));
        }
        // 8.
        let colspan = current_cell
            .col_span()
            .and_then(|x| x.parse().ok())
            .map(|x| clamp(x, 1, 1000))
            .unwrap_or(1);
        // 9.
        let rowspan = current_cell
            .row_span()
            .and_then(|x| x.parse().ok())
            .map(|x| clamp(x, 1, 65534))
            .unwrap_or(1);
        // 10.
        // ignore quirks mode
        let cell_grows_downward = false;
        // 11.
        chmax(&mut context.x_width, context.x_current + colspan);
        // 12.
        chmax(&mut context.y_height, context.y_current + rowspan);
        // 13.
        let cell = parsed::Cell {
            contents: current_cell.contents(),
            col_span: context.x_current..context.x_current + colspan,
            row_span: context.y_current..context.y_current + rowspan,
        };
        context.assigned_slots.extend(
            cell.col_span
                .clone()
                .cartesian_product(cell.row_span.clone()),
        );
        context.cells.push(cell);
        // TODO whether header or not
        algorithm_for_assigning_header_cells(context);
        // TODO check for overlap
        // 14.
        if cell_grows_downward {
            // TODO: ignore quirks mode, thus this will not be executed
            // context.list_of_downward_growing_cells.push((current_cell, x_current, colspan));
        }
        // 15.
        context.x_current += colspan;
    }
    // 16.
    context.y_current += 1;
}

fn algorithm_for_growing_downward_growing_cells<T>(_context: &mut Context<T>) {
    // ignore quirks mode, thus this will not be executed
}

fn algorithm_for_assigning_header_cells<T>(_context: &mut Context<T>) {
    // do I really need this?
}

#[cfg(test)]
mod tests {
    use super::CellKind::*;
    use crate::html5_table_parser::{
        CellKind, CellLike, RowLike, TRGroupLike, TableChild, TableLike,
    };
    use itertools::Itertools;

    struct Table(Vec<TableChild<RowGroup, Row>>);

    impl TableLike for Table {
        type Group = RowGroup;
        // type I = dyn Iterator<Item=TableChild<RowGroup, Row>>;

        fn children(&self) -> Vec<Option<TableChild<&RowGroup, &Row>>> {
            self.0
                .iter()
                .map(|x| match x {
                    TableChild::ColGroup => TableChild::ColGroup,
                    TableChild::THead(e) => TableChild::THead(e),
                    TableChild::TBody(e) => TableChild::TBody(e),
                    TableChild::TFoot(e) => TableChild::TFoot(e),
                    TableChild::TR(e) => TableChild::TR(e),
                })
                .map(Some)
                .collect_vec()
        }
    }

    struct RowGroup(Vec<Row>);

    impl TRGroupLike for RowGroup {
        type Row = Row;

        fn children(&self) -> Vec<Option<&Self::Row>> {
            self.0.iter().map(Some).collect_vec()
        }
    }

    struct Row(Vec<Cell>);

    impl RowLike for Row {
        type Cell = Cell;

        fn children(&self) -> Vec<Option<&Self::Cell>> {
            self.0.iter().map(Some).collect_vec()
        }
    }

    struct Cell(
        &'static str,
        CellKind,
        Option<&'static str>,
        Option<&'static str>,
    );

    impl CellLike for Cell {
        type Item = &'static str;

        fn contents(&self) -> Self::Item {
            self.0
        }

        fn kind(&self) -> CellKind {
            self.1
        }

        fn row_span(&self) -> Option<&str> {
            self.2
        }

        fn col_span(&self) -> Option<&str> {
            self.3
        }
    }

    //noinspection DuplicatedCode
    #[test]
    fn test() {
        let table = Table(vec![
            super::TableChild::TR(Row(vec![
                Cell("hoge", CellKind::TD, Some("1"), Some("1")),
                Cell("fuga", CellKind::TD, Some("1"), Some("1")),
            ])),
            super::TableChild::TR(Row(vec![
                Cell("piyo", CellKind::TD, Some("1"), Some("1")),
                Cell("foo", CellKind::TD, Some("1"), Some("1")),
            ])),
        ]);
        let table = super::form_table(table);
        assert_eq!(
            table,
            super::parsed::Table {
                rows: vec![
                    (
                        0,
                        super::parsed::Row {
                            cells: vec![
                                (
                                    0,
                                    super::parsed::Cell {
                                        contents: "hoge",
                                        row_span: 0..1,
                                        col_span: 0..1,
                                    }
                                ),
                                (
                                    1,
                                    super::parsed::Cell {
                                        contents: "fuga",
                                        row_span: 0..1,
                                        col_span: 1..2,
                                    }
                                ),
                            ]
                            .into_iter()
                            .collect()
                        }
                    ),
                    (
                        1,
                        super::parsed::Row {
                            cells: vec![
                                (
                                    0,
                                    super::parsed::Cell {
                                        contents: "piyo",
                                        row_span: 1..2,
                                        col_span: 0..1,
                                    }
                                ),
                                (
                                    1,
                                    super::parsed::Cell {
                                        contents: "foo",
                                        row_span: 1..2,
                                        col_span: 1..2,
                                    }
                                ),
                            ]
                            .into_iter()
                            .collect()
                        }
                    ),
                ]
                .into_iter()
                .collect()
            }
        );
    }

    #[test]
    fn test_conversion_with_row_span_and_col_span() {
        let table = Table(vec![
            super::TableChild::THead(RowGroup(vec![
                Row(vec![
                    Cell("Grade.", TH, Some("2"), None),
                    Cell("Yield Point.", TH, Some("2"), None),
                    Cell("Ultimate tensile strength", TH, None, Some("2")),
                    Cell("Per cent elong. 50.8mm or 2 in.", TH, Some("2"), None),
                    Cell("Per cent reduct. area.", TH, Some("2"), None),
                ]),
                Row(vec![
                    Cell("kg/mm<sup>2</sup>", TH, None, None),
                    Cell("lb/in<sup>2", TH, None, None),
                ]),
            ])),
            super::TableChild::TBody(RowGroup(vec![
                Row(vec![
                    Cell("Hard", TD, None, None),
                    Cell("0.45 ultimate", TD, None, None),
                    Cell("56.2", TD, None, None),
                    Cell("80,000", TD, None, None),
                    Cell("15", TD, None, None),
                    Cell("20", TD, None, None),
                ]),
                Row(vec![
                    Cell("Medium", TD, None, None),
                    Cell("0.45 ultimate", TD, None, None),
                    Cell("49.2", TD, None, None),
                    Cell("70,000", TD, None, None),
                    Cell("18", TD, None, None),
                    Cell("25", TD, None, None),
                ]),
                Row(vec![
                    Cell("Soft", TD, None, None),
                    Cell("0.45 ultimate", TD, None, None),
                    Cell("42.2", TD, None, None),
                    Cell("60,000", TD, None, None),
                    Cell("22", TD, None, None),
                    Cell("30", TD, None, None),
                ]),
            ])),
        ]);
        let table = super::form_table(table);
        assert_eq!(
            table,
            super::parsed::Table {
                rows: vec![
                    (
                        0,
                        super::parsed::Row {
                            cells: vec![
                                (
                                    0,
                                    super::parsed::Cell {
                                        contents: "Grade.",
                                        row_span: 0..2,
                                        col_span: 0..1,
                                    }
                                ),
                                (
                                    1,
                                    super::parsed::Cell {
                                        contents: "Yield Point.",
                                        row_span: 0..2,
                                        col_span: 1..2,
                                    }
                                ),
                                (
                                    2,
                                    super::parsed::Cell {
                                        contents: "Ultimate tensile strength",
                                        row_span: 0..1,
                                        col_span: 2..4,
                                    }
                                ),
                                (
                                    4,
                                    super::parsed::Cell {
                                        contents: "Per cent elong. 50.8mm or 2 in.",
                                        row_span: 0..2,
                                        col_span: 4..5,
                                    }
                                ),
                                (
                                    5,
                                    super::parsed::Cell {
                                        contents: "Per cent reduct. area.",
                                        row_span: 0..2,
                                        col_span: 5..6,
                                    }
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        }
                    ),
                    (
                        1,
                        super::parsed::Row {
                            cells: vec![
                                (
                                    2,
                                    super::parsed::Cell {
                                        contents: "kg/mm<sup>2</sup>",
                                        row_span: 1..2,
                                        col_span: 2..3,
                                    }
                                ),
                                (
                                    3,
                                    super::parsed::Cell {
                                        contents: "lb/in<sup>2",
                                        row_span: 1..2,
                                        col_span: 3..4,
                                    }
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        }
                    ),
                    (
                        2,
                        super::parsed::Row {
                            cells: vec![
                                (
                                    0,
                                    super::parsed::Cell {
                                        contents: "Hard",
                                        row_span: 2..3,
                                        col_span: 0..1,
                                    }
                                ),
                                (
                                    1,
                                    super::parsed::Cell {
                                        contents: "0.45 ultimate",
                                        row_span: 2..3,
                                        col_span: 1..2,
                                    }
                                ),
                                (
                                    2,
                                    super::parsed::Cell {
                                        contents: "56.2",
                                        row_span: 2..3,
                                        col_span: 2..3,
                                    }
                                ),
                                (
                                    3,
                                    super::parsed::Cell {
                                        contents: "80,000",
                                        row_span: 2..3,
                                        col_span: 3..4,
                                    }
                                ),
                                (
                                    4,
                                    super::parsed::Cell {
                                        contents: "15",
                                        row_span: 2..3,
                                        col_span: 4..5,
                                    }
                                ),
                                (
                                    5,
                                    super::parsed::Cell {
                                        contents: "20",
                                        row_span: 2..3,
                                        col_span: 5..6,
                                    }
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        }
                    ),
                    (
                        3,
                        super::parsed::Row {
                            cells: vec![
                                (
                                    0,
                                    super::parsed::Cell {
                                        contents: "Medium",
                                        row_span: 3..4,
                                        col_span: 0..1,
                                    }
                                ),
                                (
                                    1,
                                    super::parsed::Cell {
                                        contents: "0.45 ultimate",
                                        row_span: 3..4,
                                        col_span: 1..2,
                                    }
                                ),
                                (
                                    2,
                                    super::parsed::Cell {
                                        contents: "49.2",
                                        row_span: 3..4,
                                        col_span: 2..3,
                                    }
                                ),
                                (
                                    3,
                                    super::parsed::Cell {
                                        contents: "70,000",
                                        row_span: 3..4,
                                        col_span: 3..4,
                                    }
                                ),
                                (
                                    4,
                                    super::parsed::Cell {
                                        contents: "18",
                                        row_span: 3..4,
                                        col_span: 4..5,
                                    }
                                ),
                                (
                                    5,
                                    super::parsed::Cell {
                                        contents: "25",
                                        row_span: 3..4,
                                        col_span: 5..6,
                                    }
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        }
                    ),
                    (
                        4,
                        super::parsed::Row {
                            cells: vec![
                                (
                                    0,
                                    super::parsed::Cell {
                                        contents: "Soft",
                                        row_span: 4..5,
                                        col_span: 0..1,
                                    }
                                ),
                                (
                                    1,
                                    super::parsed::Cell {
                                        contents: "0.45 ultimate",
                                        row_span: 4..5,
                                        col_span: 1..2,
                                    }
                                ),
                                (
                                    2,
                                    super::parsed::Cell {
                                        contents: "42.2",
                                        row_span: 4..5,
                                        col_span: 2..3,
                                    }
                                ),
                                (
                                    3,
                                    super::parsed::Cell {
                                        contents: "60,000",
                                        row_span: 4..5,
                                        col_span: 3..4,
                                    }
                                ),
                                (
                                    4,
                                    super::parsed::Cell {
                                        contents: "22",
                                        row_span: 4..5,
                                        col_span: 4..5,
                                    }
                                ),
                                (
                                    5,
                                    super::parsed::Cell {
                                        contents: "30",
                                        row_span: 4..5,
                                        col_span: 5..6,
                                    }
                                ),
                            ]
                            .into_iter()
                            .collect(),
                        }
                    ),
                ]
                .into_iter()
                .collect(),
            }
        )
    }
}
