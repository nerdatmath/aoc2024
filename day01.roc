app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.8.0/PCkJq9IGyIpMfwuW-9hjfXd6x-bHb1_OZdacogpBcPM.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.0/je3X2cSdUa6b24fO1SS_vGNS5MwU-a-3r1niP_7iG6k.tar.br",
    ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
}

import parser.Parser
import parser.String

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

part1: Str -> Result Str _
part1 = \input ->
    String.parseStr parser input
    |> Result.map \(a, b) ->
        List.map2 (List.sortAsc a) (List.sortAsc b) Num.absDiff
        |> List.sum
        |> Num.toStr

examplePart2 = examplePart1

expect part2 examplePart2 == Ok "31"

counts: List U64 -> Dict U64 U64
counts = \list ->
    List.walk list (Dict.empty {}) \dict, item ->
        Dict.update dict item \existing -> Ok ((Result.withDefault existing 0) + 1)

expect counts [1, 1] == Dict.single 1 2

getOrDie : Dict k v, k -> v
getOrDie = \dict, key ->
    when Dict.get dict key is
        Err _ -> crash "unreached"
        Ok value -> value

dictIntersectionWalk = \d1, d2, state, f ->
    k1 = Dict.keys d1 |> Set.fromList
    k2 = Dict.keys d2 |> Set.fromList
    Set.intersection k1 k2
    |> Set.walk state \s, k ->
        f s k (getOrDie d1 k) (getOrDie d2 k)

part2 = \input ->
    String.parseStr parser input
    |> Result.map \(a, b) ->
        dictIntersectionWalk (counts a) (counts b) 0
            \sum, k, c1, c2 ->
                sum + k * c1 * c2
        |> Num.toStr
