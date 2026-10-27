# SPDX-License-Identifier: UPL-1.0
# Copyright (c) 2026 Simon Redding

import sfmt.Fmt

## A text table formatter.
##
## Build a `Table` from one of the style constructors ([Table.plain],
## [Table.pipe] or [Table.grid]), optionally describe its columns with
## [Table.with_columns], then call [Table.format] with the rows to get back
## one string per output line.
##
## Columns are sized to fit their widest cell. Rows shorter than the table are
## padded using each column's default value, and the table grows extra unnamed
## columns if a row is wider than the columns you declared.
##
## ```roc
## expect {
##     Table.pipe
##         .with_columns([Table.column("Dog"), Table.column("Cat")])
##         .format([["one", "two"], ["five", "six"]])
##         == [
##             "| Dog  | Cat |",
##             "|:-----|:----|",
##             "| one  | two |",
##             "| five | six |",
##         ]
## }
## ```
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

    ## A table drawn in the `Plain` style, with no columns declared.
    ##
    ## Without columns there is no header row, so the output is just the aligned
    ## rows.
    ##
    ## ```roc
    ## expect Table.plain.format([["one", "two"], ["five", "six"]]) == [
    ##     "one  two",
    ##     "five six",
    ## ]
    ## ```
    plain : {} -> Table
    plain = |{}| Table.{ style: Plain }

    ## A table drawn in the `Pipe` style (Markdown pipe tables), with no columns
    ## declared.
    ##
    ## ```roc
    ## expect Table.pipe.format([["one", "two"], ["five", "six"]]) == [
    ##     "| one  | two |",
    ##     "| five | six |",
    ## ]
    ## ```
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
        header : Str ?? "",
        fmt: row -> Try(Str, [OutOfBounds]),
        align: [Left, Right, Center] ?? Left,
        default: Str ?? "",
    }.{}

    ## Returns a copy of the table with its columns replaced by `columns`.
    ##
    ## The columns give the header names, alignment and default values, in
    ## left-to-right order.
    ##
    ## ```roc
    ## Table.grid.with_columns([Table.column("Dog"), Table.column("Cat")])
    ## ```
    with_columns : Table(row), List(Column(row)) -> Table
    with_columns = |table, columns| { ..table, columns }

    format : Table(row), List(row) -> List(Str)
    format = |table, rows| {
        rs = rows.map(|row| table.columns.map(|col| (col.fmt)(row).ok_or(col.default)))
        headers = table.columns.map(|col| col.header)
        max_widths = rs.fold(headers.map(Fmt.width), |mws, cells| mws.map2(cells, |mw, cell| mw.max(Fmt.width(cell))))
        rs1 = rs.map(|row| row.map3(max_widths, table.columns, |cell, mw, col| (Fmt.str |> Fmt.fit({ width: mw, align: col.align }))(cell)))
        match table.style {
            Plain => render_plain(max_widths, table.columns, rs1)
            Pipe => render_pipe(max_widths, table.columns, rs1)
            Grid => render_grid(max_widths, table.columns, rs1)
        }
    }
}

render_plain : List(U64), List(Table.Column), List(List(Str)) -> List(Str)
render_plain = |widths, cols, rows|
    (if cols.any(|col| col.header.is_empty().not())
        [
            "${cols.map2(widths, |col, mw| (Fmt.str |> Fmt.fit({ width: mw }))(col.header)) |> Str.join_with(" ")}",
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
            rule,
            "| ${cols.map2(widths, |col, mw| (Fmt.str |> Fmt.fit({ width: mw }))(col.header)) |> Str.join_with(" | ")} |",
            "+=${widths.map(|w| "=".repeat(w)) |> Str.join_with("=+=")}=+",
        ]
    else
        []
    ).concat(rows.map(|row| "| ${row |> Str.join_with(" | ")} |")
        .intersperse(rule)).append(rule)
}

render_pipe : List(U64), List(Table.Column), List(List(Str)) -> List(Str)
render_pipe = |widths, cols, rows|
    (if cols.any(|col| col.header.is_empty().not())
        [
            "| ${cols.map2(widths, |col, mw| (Fmt.str |> Fmt.fit({ width: mw }))(col.header)) |> Str.join_with(" | ")} |",
            "|${widths.map2(cols, |w, col| pipe_header_rule_cell(w, col.align)) |> Str.join_with("|")}|",
        ]
    else
        []
    ).concat(rows.map(|row| "| ${row |> Str.join_with(" | ")} |"))

pipe_header_rule_cell : U64, [Left, Right, Center] -> Str
pipe_header_rule_cell = |width, align|
    match align {
        Left => ":${"-".repeat(width + 1)}"
        Right => "${"-".repeat(width + 1)}:"
        Center => ":${"-".repeat(width)}:"
    }

expect ["one two three four"] == Table.plain({})
    .with_columns([
        { fmt: |r| r.get(0).map_ok(Fmt.str)},
        { fmt: |r| r.get(1).map_ok(Fmt.str)},
        { fmt: |r| r.get(2).map_ok(Fmt.str)},
        { fmt: |r| r.get(3).map_ok(Fmt.str)},
    ])
    .format([["one", "two", "three", "four"]])

expect {
    expected = [
        "one  two three four",
        "five six seven eight",
    ]
    actual = Table.plain({})
        .with_columns([
            { fmt: |r| r.get(0).map_ok(Fmt.str)},
            { fmt: |r| r.get(1).map_ok(Fmt.str)},
            { fmt: |r| r.get(2).map_ok(Fmt.str)},
            { fmt: |r| r.get(3).map_ok(Fmt.str)},
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    expected == actual
}

expect {
    [
        "| one  | two | three | four  |",
        "| five | six | seven | eight |",
    ] == Table.pipe({})
        .with_columns([
            { fmt: |r| r.get(0).map_ok(Fmt.str)},
            { fmt: |r| r.get(1).map_ok(Fmt.str)},
            { fmt: |r| r.get(2).map_ok(Fmt.str)},
            { fmt: |r| r.get(3).map_ok(Fmt.str)},
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
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
        .with_columns((0..<3).iter().map(|i| { fmt: |r| r.get(i).map_ok(Fmt.str) }).collect())
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
            { header: "Dog", fmt: |r| r.get(0).map_ok(Fmt.str) },
            { header: "Cat", fmt: |r| r.get(1).map_ok(Fmt.str) },
            { header: "Antelope", fmt: |r| r.get(2).map_ok(Fmt.str) },
            { header: "Sheep", fmt: |r| r.get(3).map_ok(Fmt.str) },
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "| Dog  | Cat | Antelope | Sheep |",
        "|:-----|:----|:---------|:------|",
        "| one  | two | three    | four  |",
        "| five | six | seven    | eight |",
    ]
    actual = Table.pipe({})
        .with_columns([
            { header: "Dog", fmt: |r| r.get(0).map_ok(Fmt.str) },
            { header: "Cat", fmt: |r| r.get(1).map_ok(Fmt.str) },
            { header: "Antelope", fmt: |r| r.get(2).map_ok(Fmt.str) },
            { header: "Sheep", fmt: |r| r.get(3).map_ok(Fmt.str) },
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "| Dog  | Cat | Antelope | Sheep |",
        "|:-----|:----|:---------|:------|",
        "| one  | two |          |       |",
        "| five | six | seven    | eight |",
    ]
    actual = Table.pipe({})
        .with_columns([
            { header: "Dog", fmt: |r| r.get(0).map_ok(Fmt.str) },
            { header: "Cat", fmt: |r| r.get(1).map_ok(Fmt.str) },
            { header: "Antelope", fmt: |r| r.get(2).map_ok(Fmt.str) },
            { header: "Sheep", fmt: |r| r.get(3).map_ok(Fmt.str) },
        ])
        .format([["one", "two"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "| Dog  | Cat | Antelope | Sheep |",
        "|:-----|:----|:---------|:------|",
        "| one  | two |          | N/A   |",
        "| five | six | seven    | eight |",
    ]
    actual = Table.pipe({})
        .with_columns([
            { header: "Dog", fmt: |r| r.get(0).map_ok(Fmt.str) },
            { header: "Cat", fmt: |r| r.get(1).map_ok(Fmt.str) },
            { header: "Antelope", fmt: |r| r.get(2).map_ok(Fmt.str) },
            { header: "Sheep", fmt: |r| r.get(3).map_ok(Fmt.str), default: "N/A" },
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
            { header: "Dog", fmt: |r| r.get(0).map_ok(Fmt.str) },
            { header: "Cat", fmt: |r| r.get(1).map_ok(Fmt.str) },
            { header: "Antelope", fmt: |r| r.get(2).map_ok(Fmt.str) },
            { header: "Sheep", fmt: |r| r.get(3).map_ok(Fmt.str) },
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
            { header: "Dog", fmt: |r| r.get(0).map_ok(Fmt.str) },
            { header: "Cat", fmt: |r| r.get(1).map_ok(Fmt.str) },
            { header: "Antelope", fmt: |r| r.get(2).map_ok(Fmt.str), align: Right },
            { header: "Sheep", fmt: |r| r.get(3).map_ok(Fmt.str) },
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "| Dog  | Cat | Antelope | Sheep |",
        "|:-----|:----|---------:|:------|",
        "| one  | two |    three | four  |",
        "| five | six |    seven | eight |",
    ]
    actual = Table.pipe({})
        .with_columns([
            { header: "Dog", fmt: |r| r.get(0).map_ok(Fmt.str) },
            { header: "Cat", fmt: |r| r.get(1).map_ok(Fmt.str) },
            { header: "Antelope", align: Right, fmt: |r| r.get(2).map_ok(Fmt.str) },
            { header: "Sheep", fmt: |r| r.get(3).map_ok(Fmt.str) },
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}
expect {
    expected = [
        "| Dog  | Cat | Antelope | Sheep |",
        "|:-----|:----|:--------:|:------|",
        "| one  | two |  three   | four  |",
        "| five | six |  seven   | eight |",
    ]
    actual = Table.pipe({})
        .with_columns([
            { header: "Dog", fmt: |r| r.get(0).map_ok(Fmt.str) },
            { header: "Cat", fmt: |r| r.get(1).map_ok(Fmt.str) },
            { header: "Antelope", align: Center, fmt: |r| r.get(2).map_ok(Fmt.str) },
            { header: "Sheep", fmt: |r| r.get(3).map_ok(Fmt.str) },
        ])
        .format([["one", "two", "three", "four"], ["five", "six", "seven", "eight"]])
    actual == expected
}

expect {
    expected = [
        "| Item     | Price |",
        "|:---------|------:|",
        "| pen      | £1.50 |",
        "| pencil   | £0.75 |",
        "| notebook | £7.00 |",
        "| ink      | £0.00 |",
    ]
    actual = Table.pipe({})
        .with_columns([
            { header: "Item", fmt: |row| row.desc |> Fmt.str |> Ok },
            { header: "Price", fmt: |row| row.price |> Fmt.gbp |> Ok, align: Right },
        ])
        .format([
            { desc: "pen", price: 1.5 },
            { desc: "pencil", price: 0.75213 },
            { desc: "notebook", price: 7},
            { desc: "ink", price: 0 },
        ])
    actual == expected
}
