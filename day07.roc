app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import parser.Parser exposing [Parser]
import parser.String exposing [parseStr, codeunit, digits]

example =
    """
    190: 10 19
    3267: 81 40 27
    83: 17 5
    156: 15 6
    7290: 6 8 6 15
    161011: 16 10 13
    192: 17 8 14
    21037: 9 7 18 13
    292: 11 6 16 20
    """

Equation : { result : U64, parameters: List U64 }

Op : { lastParameter : U64, result : U64} -> Result U64 [Impossible]

addOp : Op
addOp = \{ lastParameter, result } ->
    if result >= lastParameter then
        Ok (result - lastParameter)
    else
        Err Impossible

mulOp : Op
mulOp = \{ lastParameter, result } ->
    if result % lastParameter == 0 then
        Ok (result // lastParameter)
    else
        Err Impossible

ops1 : List Op
ops1 = [addOp, mulOp]

expect couldBeTrue ops1 { result: 190, parameters: [10, 19] }
expect couldBeTrue ops1 { result: 83, parameters: [17, 5] } |> Bool.not
expect couldBeTrue ops1 { result: 3267, parameters: [81, 40, 27] }
expect couldBeTrue ops1 { result: 156, parameters: [15, 6] } |> Bool.not

couldBeTrue : List Op, Equation -> Bool
couldBeTrue = \ops, { result, parameters } ->
    when parameters is
        [] -> Bool.false
        [only] -> result == only
        [.. as rest, lastParameter] ->
            List.any ops \op ->
                when op { lastParameter, result } is
                    Ok newResult ->
                        couldBeTrue ops { result: newResult, parameters: rest }
                    Err Impossible ->
                        Bool.false

space : Parser String.Utf8 {}
space = codeunit ' ' |> Parser.ignore

newline : Parser String.Utf8 {}
newline = codeunit '\n' |> Parser.ignore

expect
    result = parseEquation |> parseStr "3267: 81 40 27"
    result == Ok { result: 3267, parameters: [ 81, 40, 27 ]}

parseEquation : Parser String.Utf8 Equation
parseEquation =
    Parser.const \result -> \parameters -> { result, parameters }
    |> Parser.keep digits
    |> Parser.skip (String.string ": ")
    |> Parser.keep
        (digits |> Parser.sepBy1 space)

parse : Parser String.Utf8 (List Equation)
parse =
    parseEquation |> Parser.sepBy1 newline

expect part1 example == Ok "3749"

part1: Str -> Result Str _
part1 = \input ->
    parse
    |> parseStr input
    |> try
    |> List.keepIf \eqn -> couldBeTrue ops1 eqn
    |> List.map .result
    |> List.sum
    |> Num.toStr
    |> Ok

numDropSuffix : U64, U64 -> Result U64 [NotSuffix]
numDropSuffix = \n, suffix ->
    if suffix == 0 then
        Ok n
    else if suffix % 10 == n % 10 then
        numDropSuffix (n // 10) (suffix // 10)
    else
        Err NotSuffix

concatOp : Op
concatOp = \{ lastParameter, result } ->
    numDropSuffix result lastParameter
    |> Result.mapErr \NotSuffix -> Impossible

ops2 : List Op
ops2 = [addOp, mulOp, concatOp]

expect couldBeTrue ops2 { result: 190, parameters: [10, 19] }
expect couldBeTrue ops2 { result: 83, parameters: [17, 5] } |> Bool.not
expect couldBeTrue ops2 { result: 3267, parameters: [81, 40, 27] }
expect couldBeTrue ops2 { result: 156, parameters: [15, 6] }
expect couldBeTrue ops2 { result: 192, parameters: [17, 8, 14] }

expect part2 example == Ok "11387"

part2: Str -> Result Str _
part2 = \input ->
    parse
    |> parseStr input
    |> try
    |> List.keepIf \eqn -> couldBeTrue ops2 eqn
    |> List.map .result
    |> List.sum
    |> Num.toStr
    |> Ok
