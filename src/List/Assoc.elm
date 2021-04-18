module List.Assoc exposing
    ( getFirst, getAll, findFirst
    , removeFirst, removeFirstWith, removeAll
    , filter
    , group, groupBy, groupWith
    )

{-| Some basic functions are missing by design:
operations like add an entry, map, concat are provided by Elm's
standard library so there is no need to have a `List.Assoc` version of them.


# Search, filter and deletion

@docs getFirst, getAll, findFirst
@docs removeFirst, removeFirstWith, removeAll
@docs filter


# Grouping

@docs group, groupBy, groupWith

-}


unwrapKey : (k -> a) -> ( k, v ) -> a
unwrapKey fn ( key, _ ) =
    fn key


{-| Gets the value of the first entry that has a given `key`.
-}
getFirst : k -> List ( k, v ) -> Maybe v
getFirst key =
    findFirst ((==) key) >> Maybe.map Tuple.second


{-| Gives the values of all entries that with a given `key`.
-}
getAll : k -> List ( k, v ) -> List v
getAll key =
    filter ((==) key) >> List.map Tuple.second


{-| Finds the first tuple with a `key` that satisfy some predicate.
-}
findFirst : (k -> Bool) -> List ( k, v ) -> Maybe ( k, v )
findFirst pred assoc =
    case assoc of
        hd :: tl ->
            if unwrapKey pred hd then
                Just hd

            else
                findFirst pred tl

        [] ->
            Nothing


removeFirstRec : (a -> Bool) -> ( List a, List a ) -> ( List a, List a )
removeFirstRec pred ( dest, src ) =
    case src of
        hd :: tl ->
            if pred hd then
                ( dest, tl )

            else
                removeFirstRec pred ( hd :: dest, tl )

        [] ->
            ( dest, src )


{-| Works like [`removeFirst`](#removeFirst), but takes a `(k -> Bool)` predicate.
-}
removeFirstWith : (k -> Bool) -> List ( k, v ) -> List ( k, v )
removeFirstWith pred assoc =
    let
        ( dest, src ) =
            removeFirstRec (unwrapKey pred) ( [], assoc )
    in
    List.reverse dest ++ src


{-| Removes the first pair with a given `key`.
-}
removeFirst : k -> List ( k, v ) -> List ( k, v )
removeFirst key =
    removeFirstWith <| (==) key


{-| Removes all entries that have some `key`.
-}
removeAll : k -> List ( k, v ) -> List ( k, v )
removeAll key =
    filter <| (/=) key


{-| The same as [List.filter](https://package.elm-lang.org/packages/elm/core/latest/List#filter),
but the test is made on the `key`.
If you need a `removeAllWith` or a `findAll` then this is your guy.
-}
filter : (k -> Bool) -> List ( k, v ) -> List ( k, v )
filter pred =
    List.filter <| unwrapKey pred


reverseGroupsRec : List ( k, List v ) -> List ( k, List v ) -> List ( k, List v )
reverseGroupsRec groups acc =
    case groups of
        ( k, v ) :: tl ->
            reverseGroupsRec tl <| ( k, List.reverse v ) :: acc

        [] ->
            acc


groupSortedRec : (k -> k -> Bool) -> List ( k, v ) -> k -> List v -> List ( k, List v ) -> List ( k, List v )
groupSortedRec equal src buffk buffv dest =
    case src of
        ( k1, v1 ) :: tl ->
            if equal buffk k1 then
                groupSortedRec equal tl buffk (v1 :: buffv) dest

            else
                groupSortedRec equal tl k1 [ v1 ] (( buffk, buffv ) :: dest)

        [] ->
            reverseGroupsRec (( buffk, buffv ) :: dest) []


groupSorted : (k -> k -> Bool) -> List ( k, v ) -> List ( k, List v )
groupSorted equal assoc =
    case assoc of
        ( k1, v1 ) :: tl ->
            groupSortedRec equal tl k1 [ v1 ] []

        [] ->
            []


{-| Sorts the list by `key` then groups together adjacent entries
with the same key.

    type alias User =
        { name : String
        , age : Int
        }

    users : List User
    users =
        [ { name = "bob", age = 20 }
        , { name = "alice", age = 25 }
        , { name = "bob", age = 32 }
        ]

    usersByName : List ( String, List User )
    usersByName =
        |> List.map (\p -> ( p.name, p ))
        |> List.Assoc.group

    usersByName ==
        [ ( "alice", [ { age = 25, name = "alice" } ] )
        , ( "bob", [ { age = 20, name = "bob" }, { age = 32, name = "bob" } ] )
        ]

-}
group : List ( comparable, v ) -> List ( comparable, List v )
group assoc =
    assoc
        |> List.sortBy Tuple.first
        |> groupSorted (==)


{-| Same as [`group`](#group), but takes a function to map a `key` to `comparable`.

Cf. [List.sortBy](https://package.elm-lang.org/packages/elm/core/latest/List#sortBy)

-}
groupBy : (k -> comparable) -> List ( k, v ) -> List ( k, List v )
groupBy comp assoc =
    assoc
        |> List.sortBy (unwrapKey comp)
        |> groupSorted (\k1 k2 -> comp k1 == comp k2)


{-| Same as [`group`](#group), but takes a custom comparison function.

Cf. [List.sortWith](https://package.elm-lang.org/packages/elm/core/latest/List#)

-}
groupWith : (k -> k -> Order) -> List ( k, v ) -> List ( k, List v )
groupWith ord assoc =
    assoc
        |> List.sortWith (\( k1, _ ) ( k2, _ ) -> ord k1 k2)
        |> groupSorted (\k1 k2 -> ord k1 k2 == EQ)
