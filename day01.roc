app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import Bag
import parser.Parser
import parser.String

parser : Parser.Parser _ (List U64, List U64)
parser =
    Parser.const \a -> \b -> (a, b)
    |> Parser.keep String.digits
    |> Parser.skip (Parser.oneOrMore (String.codeunit ' '))
    |> Parser.keep String.digits
    |> Parser.sepBy (String.codeunit '\n')
    |> Parser.map unzip

unzip: List (a, b) -> (List a, List b)
unzip = \list ->
    list
    |> List.walk ([], [])
        \(xs, ys), (x, y) -> (List.append xs x, List.append ys y)

examplePart1 = Str.trim
    """
    3   4
    4   3
    2   5
    1   3
    3   9
    3   3
    """

expect part1 examplePart1 == Ok "11"

part1: Str -> Result Str _
part1 = \input ->
    String.parseStr parser input
    |> try
    |> \(xs, ys) -> List.map2 (List.sortAsc xs) (List.sortAsc ys) Num.absDiff
    |> List.sum
    |> Num.toStr
    |> Ok
    |> Result.mapErr \_ -> ThisLineIsNecessaryForRoc

examplePart2 = examplePart1

expect part2 examplePart2 == Ok "31"

part2: Str -> Result Str _
part2 = \input ->
    String.parseStr parser input
    |> try
    |> \(xs, ys) -> Bag.combine (Bag.fromList xs) (Bag.fromList ys) Num.mul
    |> Bag.map \k, count -> k*count
    |> Bag.values
    |> List.sum
    |> Num.toStr
    |> Ok
    |> Result.mapErr \_ -> ThisLineIsNecessaryForRoc
