app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import parser.Parser exposing [Parser]
import parser.String exposing [parseStr, codeunit]

example =
    """
    47|53
    97|13
    97|61
    97|47
    75|29
    61|13
    75|53
    29|13
    97|29
    53|29
    61|53
    97|53
    61|29
    47|13
    75|47
    97|75
    47|61
    75|61
    47|29
    75|13
    53|13

    75,47,61,53,29
    97,61,53,29,13
    75,29,13
    75,97,47,61,53
    61,13,29
    97,13,75,29,47
    """

Page := U64 implements [ Eq, Hash, Inspect ]

expect parsePage
    |> parseStr "97"
    |> Bool.isEq (Ok (@Page 97))

parsePage : Parser String.Utf8 Page
parsePage = Parser.map String.digits @Page

Rule : { before : Page, after: Page }

expect parseRule
    |> parseStr "97|75"
    |> Bool.isEq (Ok { before: @Page 97, after: @Page 75 })

parseRule : Parser String.Utf8 Rule
parseRule =
    Parser.const \before -> \after -> { before, after }
    |> Parser.keep parsePage
    |> Parser.skip (codeunit '|')
    |> Parser.keep parsePage

Update : List Page

expect parseUpdate
    |> parseStr "75,29,13"
    |> Bool.isEq (Ok [ @Page 75, @Page 29, @Page 13 ])

parseUpdate : Parser String.Utf8 Update
parseUpdate =
    parsePage
    |> Parser.sepBy1 (codeunit ',')

expect parse
    |> parseStr example
    |> Result.isOk

parse : Parser String.Utf8 { rules : Set Rule, updates: List Update }
parse =
    Parser.const \rules -> \updates -> { rules: Set.fromList rules, updates }
    |> Parser.keep (Parser.sepBy1 parseRule (codeunit '\n'))
    |> Parser.skip (String.string "\n\n")
    |> Parser.keep (Parser.sepBy1 parseUpdate (codeunit '\n'))

expect middle [ @Page 1, @Page 2, @Page 3] == Ok (@Page 2)

middle : Update -> Result Page _
middle = \update ->
    when update is
        [] -> Err EvenPageCount
        [mid] -> Ok mid
        [_, .. as mid, _] -> middle mid

expect part1 example == Ok "143"

part1 : Str -> Result Str _
part1 = \input ->
    parse
    |> parseStr input
    |> try
    |> \{ rules, updates } ->
        updates
        |> List.keepIf (check rules)
        |> List.mapTry middle
        |> try
        |> List.map \@Page p -> p
        |> List.sum
        |> Num.toStr
        |> Ok

cmp : Set Rule -> (Page, Page -> [ LT, EQ, GT ])
cmp = \rules -> \x, y ->
    if Set.contains rules { before: x, after: y } then
        LT
    else if x == y then
        EQ
    else
        GT

check : Set Rule -> (Update -> Bool)
check = \rules -> \update ->
    List.map2 (List.dropLast update 1) (List.dropFirst update 1) (cmp rules)
    |> List.all (\c -> c == LT)

expect part2 example == Ok "123"

part2 : Str -> Result Str _
part2 = \input ->
    parse
    |> parseStr input
    |> try
    |> \{ rules, updates } ->
        updates
        |> List.dropIf (check rules)
        |> List.map (\update -> List.sortWith update (cmp rules))
        |> List.mapTry middle
        |> try
        |> List.map \@Page p -> p
        |> List.sum
        |> Num.toStr
        |> Ok
