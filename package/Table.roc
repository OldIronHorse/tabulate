# SPDX-License-Identifier: UPL-1.0
# Copyright (c) 2026 Simon Redding

import sfmt.Fmt

## A text table formatter.
##
## Build a `Table` from one of the style constructors ([Table.plain],
## [Table.pipe] or [Table.grid]), describe its columns with
## [Table.with_columns], then call [Table.format] with the rows to get back
## one string per output line.
##
## Columns are sized to fit their widest cell. Rows shorter than the table are
## padded using each column's default value.
Table(row) :: {
    style: Style,
    columns: List(Column(row)) ?? [],
}.{
    ## How a table is drawn.
    ##
    ## - `Plain`: columns separated by spaces, with a dashed rule under the header.
    ## - `Pipe`: Markdown-style pipe table, including alignment markers.
    ## - `Grid`: reStructuredText-style grid with a rule between every row.
    Style : [Plain, Pipe, Grid]

    ## A table drawn in the `Plain` style
    ##
    ## With empty header names there is no header row, so the output is just
    ## the aligned rows.
    plain : {} -> Table
    plain = |{}| Table.{ style: Plain }

    ## A table drawn in the `Pipe` style (Markdown pipe tables)
    pipe : {} -> Table
    pipe = |{}| Table.{ style: Pipe }

    ## A table drawn in the `Grid` style, with a rule above, below and between
    ## every row, with no columns declared.
    ##
    ## When columns are declared, the header is separated from the body by a
    ## double (`=`) rule.
    grid : {} -> Table
    grid = |{}| Table.{ style: Grid }

    ## Describes a single column of a [Table]: its header name, source from each
    ## row and its alignment and formatting
    ##
    ## Create one with [Table.column]
    Column(row) := {
        header : Str,
        fmt: row -> Str,
        align: [Left, Right, Center],
        max_width: U64,
    }.{}

    ColumnOpts := {
        align: [Left, Right, Center] ?? Left,
        max_width: U64 ?? U64.highest
    }

    column : Str, (row -> Str), ColumnOpts -> Column
    column = |header, fmt, opts|
        { header, fmt, align: opts.align, max_width: opts.max_width }

    ListColumnOpts := {
        align: [Left, Right, Center] ?? Left,
        max_width: U64 ?? U64.highest
        default: Str ?? ""
    }

    col_at : U64, Str, (cell -> Str), ListColumnOpts -> Column
    col_at = |index, header, fmt, opts|
        Table.column(header, |r| r.get(index).map_ok(fmt).ok_or(opts.default), { align: opts.align, max_width: opts.max_width })

    ## Returns a copy of the table with its columns replaced by `columns`.
    ##
    ## The columns give the header names, alignment, default values, and
    ## accessor/format functions.
    ##
    ## ```roc
    ## Table.grid({}).with_columns([
    ##  Table.column({ header: "Name", |item| item.size |> Fmt.str |> Ok }),
    ##  Table.column({ header: "Price", |item| item.price |> Fmt.gbp |> Ok }),
    ## ])
    ## ```
    with_columns : Table(row), List(Column(row)) -> Table
    with_columns = |table, columns| { ..table, columns }

    format : Table(row), List(row) -> List(Str)
    format = |table, rows| {
        rs = rows.map(|row| table.columns.map(|col| (col.fmt)(row)))
        headers = table.columns.map(|col| col.header)
        max_widths = rs
            .fold(headers.map(Fmt.width), |mws, cells|
                mws.map2(cells, |mw, cell| mw.max(Fmt.width(cell))))
            .map2(table.columns, |mw, col| mw.min(col.max_width))
        rs1 = rs.map(|row| row.map3(max_widths, table.columns, |cell, mw, col| (Fmt.str |> Fmt.fit({ width: mw, align: col.align }))(cell)))
        match table.style {
            Plain => render_plain(max_widths, table.columns, rs1)
            Pipe =>
                render_pipe(table.columns, rs.map(|r|
                    r.map(|cell| cell.replace_each("|", "\\|").replace_each("\n", "<br>"))))
            Grid => render_grid(max_widths, table.columns, rs1)
        }
    }
}

render_plain : List(U64), List(Table.Column), List(List(Str)) -> List(Str)
render_plain = |widths, cols, rows|
    (if cols.any(|col| col.header.is_empty().not())
        [
            "${cols.map2(widths, |col, mw| (Fmt.str |> Fmt.fit({ width: mw }))(col.header)) |> Str.join_with(" ")}".trim_end(),
            "${widths.map(|w| "-".repeat(w)) |> Str.join_with(" ")}",
        ]
    else
        []
    ).concat(rows.map(|row| row |> Str.join_with(" ") |> Str.trim_end()))

render_grid : List(U64), List(Table.Column), List(List(Str)) -> List(Str)
render_grid = |widths, cols, rows| {
    rule = "+-${widths.map(|w| "-".repeat(w)) |> Str.join_with("-+-")}-+"
    (if cols.any(|col| col.header.is_empty().not())
        [
            "| ${cols.map2(widths, |col, mw| (Fmt.str |> Fmt.fit({ width: mw }))(col.header)) |> Str.join_with(" | ")} |",
            "+=${widths.map(|w| "=".repeat(w)) |> Str.join_with("=+=")}=+",
        ]
    else
        []
    ).concat(rows.map(|row| "| ${row |> Str.join_with(" | ")} |")
        .intersperse(rule)).append(rule).prepend(rule)
}

render_pipe : List(Table.Column), List(List(Str)) -> List(Str)
render_pipe = |cols, rows| [
    "|${cols.map(|col| Fmt.str(col.header)) |> Str.join_with("|")}|",
    "|${cols.map(|col| pipe_header_rule_cell(col.align)) |> Str.join_with("|")}|",
].concat(rows.map(|row| "|${row |> Str.join_with("|")}|"))

pipe_header_rule_cell : [Left, Right, Center] -> Str
pipe_header_rule_cell = |align|
    match align {
        Left => ":--"
        Right => "--:"
        Center => ":-:"
    }

expect ["one two three four"] == Table.plain({})
    .with_columns([
        Table.col_at(0, "", Fmt.str, {}),
        Table.col_at(1, "", Fmt.str, {}),
        Table.col_at(2, "", Fmt.str, {}),
        Table.col_at(3, "", Fmt.str, {}),
    ])
    .format([["one", "two", "three", "four"]])

expect {
    expected = [
        "one  two three four",
        "five six seven eight",
    ]
    actual = Table.plain({})
        .with_columns([
            Table.col_at(0, "", Fmt.str, {}),
            Table.col_at(1, "", Fmt.str, {}),
            Table.col_at(2, "", Fmt.str, {}),
            Table.col_at(3, "", Fmt.str, {}),
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    expected == actual
}

expect {
    expected = [
        "|||||",
        "|:--|:--|:--|:--|",
        "|one|two|three|four|",
        "|five|six|seven|eight|",
    ]
    actual =Table.pipe({})
        .with_columns([
            Table.col_at(0, "", Fmt.str, {}),
            Table.col_at(1, "", Fmt.str, {}),
            Table.col_at(2, "", Fmt.str, {}),
            Table.col_at(3, "", Fmt.str, {}),
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "-E, --extended-regexp  PATTERNS are extended regular expressions",
        "-f, --file=FILE        take PATTERNS from FILE",
        "    --max-count=NUM    stop after NUM lines selected",
        "",
        "    --help             display this help text and exit",
    ]
    actual = Table.plain({})
        .with_columns((0..<3).iter().map(|i| Table.col_at(i, "", Fmt.str, {})).collect())
        .format([
            ["-E,", "--extended-regexp", " PATTERNS are extended regular expressions"],
            ["-f,", "--file=FILE", " take PATTERNS from FILE"],
            ["", "--max-count=NUM", " stop after NUM lines selected"],
            [""],
            ["", "--help", " display this help text and exit"],
        ])
    actual == expected
}

expect {
    expected = [
        "Dog  Cat Antelope Sheep",
        "---- --- -------- -----",
        "one  two three    four",
        "five six seven    eight",
    ]
    actual = Table.plain({})
        .with_columns([
            Table.col_at(0, "Dog", Fmt.str, {}),
            Table.col_at(1, "Cat", Fmt.str, {}),
            Table.col_at(2, "Antelope", Fmt.str, {}),
            Table.col_at(3, "Sheep", Fmt.str, {}),
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "|Dog|Cat|Antelope|Sheep|",
        "|:--|:--|:--|:--|",
        "|one|two|three|four|",
        "|five|six|seven|eight|",
    ]
    actual = Table.pipe({})
        .with_columns([
            Table.col_at(0, "Dog", Fmt.str, {}),
            Table.col_at(1, "Cat", Fmt.str, {}),
            Table.col_at(2, "Antelope", Fmt.str, {}),
            Table.col_at(3, "Sheep", Fmt.str, {}),
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "|Dog|Cat|Antelope|Sheep|",
        "|:--|:--|:--|:--|",
        "|one|two|||",
        "|five|six|seven|eight|",
    ]
    actual = Table.pipe({})
        .with_columns([
            Table.col_at(0, "Dog", Fmt.str, {}),
            Table.col_at(1, "Cat", Fmt.str, {}),
            Table.col_at(2, "Antelope", Fmt.str, {}),
            Table.col_at(3, "Sheep", Fmt.str, {}),
        ])
        .format([["one", "two"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "|Dog|Cat|Antelope|Sheep|",
        "|:--|:--|:--|:--|",
        "|one|two||N/A|",
        "|five|six|seven|eight|",
    ]
    actual = Table.pipe({})
        .with_columns([
            Table.col_at(0, "Dog", Fmt.str, {}),
            Table.col_at(1, "Cat", Fmt.str, {}),
            Table.col_at(2, "Antelope", Fmt.str, {}),
            Table.col_at(3, "Sheep", Fmt.str, { default: "N/A" }),
        ])
        .format([["one", "two"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "+------+-----+----------+-------+",
        "| Dog  | Cat | Antelope | Sheep |",
        "+======+=====+==========+=======+",
        "| one  | two | three    | four  |",
        "+------+-----+----------+-------+",
        "| five | six | seven    | eight |",
        "+------+-----+----------+-------+",
    ]
    actual = Table.grid({})
        .with_columns([
            Table.col_at(0, "Dog", Fmt.str, {}),
            Table.col_at(1, "Cat", Fmt.str, {}),
            Table.col_at(2, "Antelope", Fmt.str, {}),
            Table.col_at(3, "Sheep", Fmt.str, {}),
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "+------+-----+-------+-------+",
        "| one  | two | three | four  |",
        "+------+-----+-------+-------+",
        "| five | six | seven | eight |",
        "+------+-----+-------+-------+",
    ]
    actual = Table.grid({})
        .with_columns([
            Table.col_at(0, "", Fmt.str, {}),
            Table.col_at(1, "", Fmt.str, {}),
            Table.col_at(2, "", Fmt.str, {}),
            Table.col_at(3, "", Fmt.str, {}),
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "Dog  Cat Antelope Sheep",
        "---- --- -------- -----",
        "one  two    three four",
        "five six    seven eight",
    ]
    actual = Table.plain({})
        .with_columns([
            Table.col_at(0, "Dog", Fmt.str, {}),
            Table.col_at(1, "Cat", Fmt.str, {}),
            Table.col_at(2, "Antelope", Fmt.str, { align: Right }),
            Table.col_at(3, "Sheep", Fmt.str, {}),
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "|Dog|Cat|Antelope|Sheep|",
        "|:--|:--|--:|:--|",
        "|one|two|three|four|",
        "|five|six|seven|eight|",
    ]
    actual = Table.pipe({})
        .with_columns([
            Table.col_at(0, "Dog", Fmt.str, {}),
            Table.col_at(1, "Cat", Fmt.str, {}),
            Table.col_at(2, "Antelope", Fmt.str, { align: Right }),
            Table.col_at(3, "Sheep", Fmt.str, {}),
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}
expect {
    expected = [
        "|Dog|Cat|Antelope|Sheep|",
        "|:--|:--|:-:|:--|",
        "|one|two|three|four|",
        "|five|six|seven|eight|",
    ]
    actual = Table.pipe({})
        .with_columns([
            Table.col_at(0, "Dog", Fmt.str, {}),
            Table.col_at(1, "Cat", Fmt.str, {}),
            Table.col_at(2, "Antelope", Fmt.str, { align: Center }),
            Table.col_at(3, "Sheep", Fmt.str, {}),
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "|Item|Price|",
        "|:--|--:|",
        "|pen|£1.50|",
        "|pencil|£0.75|",
        "|notebook|£7.00|",
        "|ink|£0.00|",
    ]
    actual = Table.pipe({})
        .with_columns([
            Table.column("Item", Fmt.str |> Fmt.map(|r| r.desc), {}),
            Table.column("Price", Fmt.gbp |> Fmt.map(|r| r.price), { align: Right }),
        ])
        .format([
            { desc: "pen", price: 1.5 },
            { desc: "pencil", price: 0.75213 },
            { desc: "notebook", price: 7},
            { desc: "ink", price: 0 },
        ])
    actual == expected
}
