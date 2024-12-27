app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import array2d.Array2D exposing [Array2D]
import array2d.Index2D exposing [Index2D]

example = Str.trim
    """
    MMMSXXMASM
    MSAMXMSMSA
    AMXSXMAAMM
    MSAMASMSMX
    XMASAMXAMM
    XXAMMXXAMA
    SMSMSASXSS
    SAXAMASAAA
    MAMMMXMMMM
    MXMXAXMASX
    """

checkDir :
    Array2D U8,
    Index2D,
    [PrevRow, SameRow, NextRow],
    [PrevCol, SameCol, NextCol],
    List U8
    -> Bool
checkDir = \arr, i, rowDir, colDir, str ->
    when str is
        [] -> Bool.true
        [ch] if Array2D.get arr i == Ok ch -> Bool.true
        [ch, .. as rest] if Array2D.get arr i == Ok ch ->
            when Index2D.adjacentTo i (Array2D.shape arr) rowDir colDir is
                Ok next -> checkDir arr next rowDir colDir rest
                _ -> Bool.false
        _ -> Bool.false

countAllDirections : Array2D U8, Index2D, List U8 -> U64
countAllDirections = \arr, i, str ->
    List.sum [
        if checkDir arr i PrevRow PrevCol str then 1 else 0,
        if checkDir arr i PrevRow SameCol str then 1 else 0,
        if checkDir arr i PrevRow NextCol str then 1 else 0,
        if checkDir arr i SameRow PrevCol str then 1 else 0,
        if checkDir arr i SameRow NextCol str then 1 else 0,
        if checkDir arr i NextRow PrevCol str then 1 else 0,
        if checkDir arr i NextRow SameCol str then 1 else 0,
        if checkDir arr i NextRow NextCol str then 1 else 0,
    ]

countXMAS : Array2D U8 -> U64
countXMAS = \arr ->
    Array2D.walk arr 0 { direction: Forwards } \sum, _, i ->
        sum + countAllDirections arr i (Str.toUtf8 "XMAS")

expect part1 example == Ok "18"

part1 : Str -> Result Str _
part1 = \input ->
    input
    |> Str.toUtf8
    |> List.splitOn '\n'
    |> Array2D.fromExactLists
    |> try
    |> countXMAS
    |> Num.toStr
    |> Ok

isXMAS2 : Array2D U8, Index2D -> Bool
isXMAS2 = \arr, i ->
    if i.row >= 1 && i.col >= 1 then
        List.all [
            Array2D.get arr i == Ok 'A',
            List.any [
                List.all [
                    Array2D.get arr {row: i.row-1, col: i.col-1} == Ok 'M',
                    Array2D.get arr {row: i.row+1, col: i.col+1} == Ok 'S',
                ] \b -> b,
                List.all [
                    Array2D.get arr {row: i.row-1, col: i.col-1} == Ok 'S',
                    Array2D.get arr {row: i.row+1, col: i.col+1} == Ok 'M',
                ] \b -> b,
            ] \b -> b,
            List.any [
                List.all [
                    Array2D.get arr {row: i.row-1, col: i.col+1} == Ok 'M',
                    Array2D.get arr {row: i.row+1, col: i.col-1} == Ok 'S',
                ] \b -> b,
                List.all [
                    Array2D.get arr {row: i.row-1, col: i.col+1} == Ok 'S',
                    Array2D.get arr {row: i.row+1, col: i.col-1} == Ok 'M',
                ] \b -> b,
            ] \b -> b,
        ] \b -> b
    else
        Bool.false

countXMAS2 : Array2D U8 -> U64
countXMAS2 = \arr ->
    Array2D.walk arr 0 { direction: Forwards } \sum, _, i ->
        sum + (if isXMAS2 arr i then 1 else 0)

expect part2 example == Ok "9"

part2 : Str -> Result Str _
part2 = \input ->
    input
    |> Str.toUtf8
    |> List.splitOn '\n'
    |> Array2D.fromExactLists
    |> try
    |> countXMAS2
    |> Num.toStr
    |> Ok
