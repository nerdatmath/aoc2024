app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
}

import parser.Parser
import parser.String

mulParser : Parser.Parser String.Utf8 U64
mulParser =
    Parser.const \x -> \y -> x * y
    |> Parser.skip (String.string "mul(")
    |> Parser.keep String.digits
    |> Parser.skip (String.codeunit ',')
    |> Parser.keep String.digits
    |> Parser.skip (String.codeunit ')')

expect mulParser |> String.parseStr "garbage" |> Result.isErr
expect mulParser |> String.parseStr "mul(3,4)" == Ok 12
expect mulParser |> String.parseStrPartial "mul(3,4)xxx" == Ok {input:"xxx", val:12}

examplePart1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"

expect part1 examplePart1 == Ok "161"

part1 : Str -> Result Str _
part1 = \input ->
    Parser.oneOf [
        mulParser,
        Parser.const 0
        |> Parser.skip String.anyCodeunit,
    ]
    |> Parser.many
    |> String.parseStr input
    |> Result.map List.sum
    |> Result.map Num.toStr

tillDo : Parser.Parser String.Utf8 {}
tillDo = Parser.buildPrimitiveParser \input ->
    when input is
        ['d', 'o', '(', ')', .. as inputRest] ->
            Ok { val: {}, input: inputRest }
        [] ->
            Ok { val: {}, input: input }
        [_, .. as inputRest] ->
            Parser.parsePartial tillDo inputRest

examplePart2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"

expect part2 examplePart2 == Ok "48"

part2 : Str -> Result Str _
part2 = \input ->
    Parser.oneOf [
        mulParser,
        Parser.const 0
        |> Parser.skip (String.string "don't()")
        |> Parser.skip tillDo,
        Parser.const 0
        |> Parser.skip String.anyCodeunit,
    ]
    |> Parser.many
    |> String.parseStr input
    |> Result.map List.sum
    |> Result.map Num.toStr
