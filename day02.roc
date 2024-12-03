app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    # parser: "https://github.com/lukewilliamboswell/roc-parser/releases/download/0.8.0/PCkJq9IGyIpMfwuW-9hjfXd6x-bHb1_OZdacogpBcPM.tar.br",
    # array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.0/je3X2cSdUa6b24fO1SS_vGNS5MwU-a-3r1niP_7iG6k.tar.br",
    # ascii: "https://github.com/Hasnep/roc-ascii/releases/download/v0.2.0/F8xZFTEm1fA7RF6OA1jl6V_ef_roDHfwGsBva29RxEg.tar.br",
}

example =
    """
    7 6 4 2 1
    1 2 7 8 9
    9 7 6 2 1
    1 3 2 4 5
    8 6 4 4 1
    1 3 6 7 9
    """

parse: Str -> Result (List (List I64)) _
parse = \input ->
    input
    |> Str.splitOn "\n"
    |> List.mapTry \line ->
        line
        |> Str.splitOn " "
        |> List.mapTry Str.toI64

expect parse example == Ok [
    [ 7, 6, 4, 2, 1, ],
    [ 1, 2, 7, 8, 9, ],
    [ 9, 7, 6, 2, 1, ],
    [ 1, 3, 2, 4, 5, ],
    [ 8, 6, 4, 4, 1, ],
    [ 1, 3, 6, 7, 9, ],
]

expect checkLevels [ 7, 6, 4, 2, 1, ] == Safe
expect checkLevels [ 1, 3, 6, 7, 9, ] == Safe
expect checkLevels [ 1, 2, 7, 8, 9, ] == Unsafe

checkLevels: List I64 -> [Safe, Unsafe]
checkLevels = \xs ->
    deltas = List.map2 xs (List.dropFirst xs 1) Num.sub
    if List.all deltas \delta -> (delta >= 1 && delta <= 3) then
        Safe
    else if List.all deltas \delta -> (delta <= -1 && delta >= -3) then
        Safe
    else
        Unsafe

expect part1 example == Ok "2"

part1: Str -> Result Str _
part1 = \input ->
    input
    |> parse
    |> try
    |> List.keepIf \levels ->
        (checkLevels levels) == Safe
    |> List.len
    |> Num.toStr
    |> Ok

expect problemDampener [1, 2, 3] == [[2, 3], [1, 3], [1, 2], [1, 2, 3]]

problemDampener: List I64 -> List (List I64)
problemDampener = \list ->
    when list is
        [] -> [[]]
        [x, .. as xs] -> List.prepend (List.map (problemDampener xs) (\dampened -> List.prepend dampened x)) xs

expect checkLevelsWithProblemDampener [ 7, 6, 4, 2, 1, ] == Safe
expect checkLevelsWithProblemDampener [ 1, 2, 7, 8, 9, ] == Unsafe
expect checkLevelsWithProblemDampener [ 9, 7, 6, 2, 1, ] == Unsafe
expect checkLevelsWithProblemDampener [ 1, 3, 2, 4, 5, ] == Safe
expect checkLevelsWithProblemDampener [ 8, 6, 4, 4, 1, ] == Safe
expect checkLevelsWithProblemDampener [ 1, 3, 6, 7, 9, ] == Safe

checkLevelsWithProblemDampener: List I64 -> [Safe, Unsafe]
checkLevelsWithProblemDampener = \list ->
    if List.any (problemDampener list) (\levels -> (checkLevels levels) == Safe) then
        Safe
    else
        Unsafe

expect part2 example == Ok "4"

part2: Str -> Result Str _
part2 = \input ->
    input
    |> parse
    |> try
    |> List.keepIf \levels ->
        (checkLevelsWithProblemDampener levels) == Safe
    |> List.len
    |> Num.toStr
    |> Ok
