app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    # parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.9.0/w8YKp2YAgQt5REYk912HfKAHBjcXsrnvtjI0CBzoAT4.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
    # ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
}

examplePart1 =
    "the example for part 1"

expect part1 examplePart1 == Ok "the example for part 1"

part1: Str -> Result Str _
part1 = \input ->
    input
    |> Ok
    |> Result.mapErr \_ -> ThisLineIsNecessaryForRoc

examplePart2 =
    "example for part 2"

expect part2 examplePart2 == Ok "2 trap rof elpmaxe"

part2: Str -> Result Str _
part2 = \input ->
    input
    |> Str.toUtf8
    |> List.reverse
    |> Str.fromUtf8
