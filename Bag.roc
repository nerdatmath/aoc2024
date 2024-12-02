module [
    Bag,
    empty,
    insert,
    fromList,
    toSet,
    count,
    combine,
    map,
    keys,
    values,
]

Bag a := Dict a U64

empty = \{} ->
    Dict.empty {}
    |> @Bag

insert = \@Bag dict, k, n ->
    dict
    |> Dict.update k \result ->
        result
        |> Result.withDefault 0
        |> \x -> x + n
        |> Ok
    |> @Bag

fromList = \list ->
    List.walk list (empty {}) \bag, k -> insert bag k 1

count = \@Bag dict, k ->
    Dict.get dict k |> Result.withDefault 0

keys = \@Bag dict ->
    Dict.keys dict

toSet = \bag ->
    keys bag |> Set.fromList

combine = \bag1, bag2, f ->
    Set.union (toSet bag1) (toSet bag2)
    |> Set.walk (empty {}) \bag, k ->
        insert bag k (f (count bag1 k) (count bag2 k))

map = \@Bag dict, f ->
    Dict.map dict f |> @Bag

values = \@Bag dict ->
    Dict.values dict
