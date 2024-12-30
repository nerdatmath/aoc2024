app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import parser.Parser exposing [Parser]
import parser.String exposing [parseStr, codeunit]
import array2d.Array2D exposing [Array2D]
import array2d.Index2D exposing [Index2D]

example =
    """
    the example
    """

expect part1 example == Ok "the example"

part1: Str -> Result Str _
part1 = \input ->
    input
    |> Ok

expect part2 example == Err NotImplemented

part2: Str -> Result Str _
part2 = \_input -> Err NotImplemented
