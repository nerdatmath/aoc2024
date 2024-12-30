app [part1, part2] {
    pf: platform "https://github.com/ostcar/roc-aoc-platform/releases/download/v0.0.6/h-Fncg-ySjnWsh6mOiuaqdkz6wwfYCPCgy64Wep58YI.tar.br",
    array2d: "https://github.com/mulias/roc-array2d/releases/download/v0.3.1/2Jqajvxn36vRryyQBSluU6Fo6vVI5yNSYmcJcyaKp0Y.tar.br",
}

import array2d.Array2D exposing [Array2D]
import array2d.Index2D exposing [Index2D]

example = Str.trim
    """
    ....#.....
    .........#
    ..........
    ..#.......
    .......#..
    ..........
    .#..^.....
    ........#.
    #.........
    ......#...
    """

Dir : [N, E, S, W]

State : { map : Array2D U8, pos : Index2D, dir : Dir }

toState : Array2D U8 -> Result State _
toState = \map ->
    pos = Array2D.findFirstIndex? map \ch -> 
        when ch is
            '^' | '>' | 'v' | '<' -> Bool.true
            _ -> Bool.false
    dir = when Array2D.get? map pos is
        '^' -> N
        '>' -> E
        'v' -> S
        '<' -> W
        _ -> crash "Impossible"
    Ok {
        map,
        pos,
        dir,
    }

turnRight : Dir -> Dir
turnRight = \dir ->
    when dir is
        N -> E
        E -> S
        S -> W
        W -> N

forward : State -> Result Index2D [Blocked, OutOfBounds]
forward = \{map, pos, dir} ->
    shape = Array2D.shape map
    newPos = Result.mapErr? (when dir is
        N -> Index2D.adjacentTo pos shape PrevRow SameCol
        E -> Index2D.adjacentTo pos shape SameRow NextCol
        S -> Index2D.adjacentTo pos shape NextRow SameCol
        W -> Index2D.adjacentTo pos shape SameRow PrevCol
    ) \OutOfBounds -> OutOfBounds
    when Array2D.get map newPos is
        Ok '#' | Ok 'O' ->
            Err Blocked
        _ ->
            Ok newPos

next : State -> Result State [End]
next = \state ->
    when forward state is
        Ok newPos -> Ok { state & pos: newPos }
        Err Blocked -> Ok { state & dir: turnRight state.dir }
        Err OutOfBounds -> Err End

runImpl : State, Set Index2D -> U64
runImpl = \state, set ->
    newSet = Set.insert set state.pos
    when next state is
        Ok newState -> runImpl newState newSet
        Err End -> Set.len newSet

run : State -> U64
run = \state ->
    runImpl state (Set.empty {})

parse : Str -> Result State _
parse = \input ->
    input
    |> Str.toUtf8
    |> List.splitOn '\n'
    |> Array2D.fromExactLists?
    |> toState

expect part1 example == Ok "41"

part1 : Str -> Result Str _
part1 = \input ->
    input
    |> parse
    |> try
    |> run
    |> Num.toStr
    |> Ok

runImpl2 : State, Set (Index2D, Dir) -> [End, Loop]
runImpl2 = \state, set ->
    when next state is
        Ok newState ->
            if Set.contains set (newState.pos, newState.dir) then
                Loop
            else
                newSet = Set.insert set (newState.pos, newState.dir)
                runImpl2 newState newSet
        Err End ->
            End

run2 : State -> [End, Loop]
run2 = \state ->
    runImpl2 state (Set.empty {})

runImpl3 : State, Set Index2D, U64 -> U64
runImpl3 = \state, track, obstacles ->
    newTrack = Set.insert track state.pos
    when forward state is
        Ok pos ->
            if ! (Set.contains track pos) then
                newMap = Array2D.set state.map pos 'O'
                if run2 { state & map: newMap } == Loop then
                    runImpl3 { state & pos } newTrack (obstacles+1)
                else
                    runImpl3 { state & pos } newTrack obstacles
            else
                runImpl3 { state & pos } newTrack obstacles
        Err Blocked ->
            runImpl3 { state & dir: turnRight state.dir } newTrack obstacles
        Err OutOfBounds ->
            obstacles

run3 : State -> U64
run3 = \state ->
    runImpl3 state (Set.empty {}) 0

expect part2 example == Ok "6"

part2 : Str -> Result Str _
part2 = \input ->
    input
    |> parse
    |> try
    |> run3
    |> Num.toStr
    |> Ok
