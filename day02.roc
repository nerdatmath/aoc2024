app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.8/lhFfiil7mQXDOB6wN-jduJQImoT8qRmoiNHDB4DVF9s.tar.br",
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

expect checkPair 7 6 == Descending
expect checkPair 1 3 == Ascending
expect checkPair 2 7 == Unsafe

checkPair: I64, I64 -> [Ascending, Descending, Unsafe]
checkPair = \x, y ->
    when y-x is
        1|2|3 -> Ascending
        -3|-2|-1 -> Descending
        _ -> Unsafe

expect checkLevels [ 7, 6, 4, 2, 1, ] == Descending
expect checkLevels [ 1, 3, 6, 7, 9, ] == Ascending
expect checkLevels [ 1, 2, 7, 8, 9, ] == Unsafe

checkLevels: List I64 -> [Ascending, Descending, Unsafe]
checkLevels = \xs ->
    safetys = List.map2 xs (List.dropFirst xs 1) checkPair
    if List.all safetys (\x -> x == Ascending) then
        Ascending
    else if List.all safetys (\x -> x == Descending) then
        Descending
    else
        Unsafe

expect part1 example == Ok "2"

part1: Str -> Result Str _
part1 = \input ->
    input
    |> parse
    |> try
    |> List.keepIf \levels ->
        when checkLevels levels is
            Ascending -> Bool.true
            Descending -> Bool.true
            Unsafe -> Bool.false
    |> List.len
    |> Num.toStr
    |> Ok

expect problemDampener [1, 2, 3] == [[2, 3], [1, 3], [1, 2], [1, 2, 3]]

problemDampener: List I64 -> List (List I64)
problemDampener = \list ->
    when list is
        [] -> [[]]
        [x, .. as xs] -> List.prepend (List.map (problemDampener xs) (\dampened -> List.prepend dampened x)) xs

expect checkLevelsWithProblemDampener [ 7, 6, 4, 2, 1, ] == Descending
expect checkLevelsWithProblemDampener [ 1, 2, 7, 8, 9, ] == Unsafe
expect checkLevelsWithProblemDampener [ 9, 7, 6, 2, 1, ] == Unsafe
expect checkLevelsWithProblemDampener [ 1, 3, 2, 4, 5, ] == Ascending
expect checkLevelsWithProblemDampener [ 8, 6, 4, 4, 1, ] == Descending
expect checkLevelsWithProblemDampener [ 1, 3, 6, 7, 9, ] == Ascending

checkLevelsWithProblemDampener: List I64 -> [Ascending, Descending, Unsafe]
checkLevelsWithProblemDampener = \list ->
    list
    |> problemDampener
    |> List.map checkLevels
    |> List.findFirst (\result -> result != Unsafe)
    |> Result.withDefault Unsafe

expect part2 example == Ok "4"

part2: Str -> Result Str _
part2 = \input ->
    input
    |> parse
    |> try
    |> List.keepIf \levels ->
        (checkLevelsWithProblemDampener levels) != Unsafe
    |> List.len
    |> Num.toStr
    |> Ok
