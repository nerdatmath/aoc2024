app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import array2d.Array2D exposing [Array2D]
import array2d.Index2D exposing [Index2D]
import array2d.Shape2D exposing [Shape2D]

example =
    """
    ............
    ........0...
    .....0......
    .......0....
    ....0.......
    ......A.....
    ............
    ............
    ........A...
    .........A..
    ............
    ............
    """

expect isNode '0'
expect isNode 'A'

isNode : U8 -> Bool
isNode = \ch ->
    '0' <= ch && ch <= '9' || 'A' <= ch && ch <= 'Z' || 'a' <= ch && ch <= 'z'

findNodes : Array2D U8 -> Dict U8 (Set Index2D)
findNodes = \map ->
    Array2D.walk
        map
        (Dict.empty {})
        { direction: Forwards }
        \dict, ch, idx ->
            if isNode ch then
                Dict.update
                    dict
                    ch
                    \possibleSet ->
                        set = Result.withDefault possibleSet (Set.empty {})
                        Ok (Set.insert set idx)
            else
                dict

walkPairs : Set k, state, (state, k, k -> state) -> state
walkPairs = \set, st, step ->
    Set.walk set st \st1, x ->
        Set.walk set st1 \st2, y ->
            if x == y then
                st2
            else
                step st2 x y

nextAntinode : Shape2D, Index2D, Index2D -> Result Index2D _
nextAntinode = \shape, { row: row1, col: col1 }, { row: row2, col: col2 } ->
    antinode = {
        row: (Num.subChecked (row2 * 2) row1 |> Result.mapErr? \Overflow -> OutOfBounds),
        col: (Num.subChecked (col2 * 2) col1 |> Result.mapErr? \Overflow -> OutOfBounds),
    }
    if Shape2D.hasIndex shape antinode then
        Ok antinode
    else
        Err OutOfBounds

allAntinodes : Shape2D, Index2D, Index2D -> Set Index2D
allAntinodes = \shape, node1, node2 ->
    nextAntinode shape node1 node2
    |> Result.map \node3 -> allAntinodes shape node2 node3
    |> Result.withDefault (Set.empty {})
    |> Set.insert node2

expect part1 example == Ok "14"

part1: Str -> Result Str _
part1 = \input ->
    map =
        input
        |> Str.toUtf8
        |> List.splitOn '\n'
        |> Array2D.fromExactLists?
    findNodes map
    |> Dict.walk
        (Set.empty {})
        \set, _, nodes ->
            walkPairs nodes set \antinodes, node1, node2 ->
                nextAntinode (Array2D.shape map) node1 node2
                |> Result.map Set.single
                |> Result.withDefault (Set.empty {})
                |> Set.union antinodes
    |> Set.len
    |> Num.toStr
    |> Ok

expect part2 example == Ok "34"

part2: Str -> Result Str _
part2 = \input ->
    map =
        input
        |> Str.toUtf8
        |> List.splitOn '\n'
        |> Array2D.fromExactLists?
    findNodes map
    |> Dict.walk
        (Set.empty {})
        \set, _, nodes ->
            walkPairs nodes set \antinodes, node1, node2 ->
                allAntinodes (Array2D.shape map) node1 node2
                |> Set.union antinodes
    |> Set.len
    |> Num.toStr
    |> Ok
