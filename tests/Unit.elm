module Unit exposing (..)

import Expect
import List.Assoc as Assoc
import Test exposing (Test, describe, test)


greaterThanZero : Int -> Bool
greaterThanZero n =
    n > 0


sign : number -> Int
sign n =
    if n < 0 then
        -1

    else if n > 0 then
        1

    else
        0


findFirst : Test
findFirst =
    describe "findFirst"
        [ test "should be Nothing on empty lists" <|
            \_ -> Expect.equal Nothing (Assoc.findFirst identity [])
        , test "should be Nothing if pred is always false" <|
            \_ -> Expect.equal Nothing (Assoc.findFirst greaterThanZero [ ( -2, 2 ), ( -1, 1 ) ])
        , test "should be Just (k, v) when pred is true" <|
            \_ -> Expect.equal (Just ( 1, 1 )) (Assoc.findFirst greaterThanZero [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ])
        , test "should fetch the first entry that matches" <|
            \_ -> Expect.equal (Just ( 1, 1 )) (Assoc.findFirst greaterThanZero [ ( 1, 1 ), ( 1, 2 ) ])
        , test "should work on a list with a single pair" <|
            \_ -> Expect.equal (Just ( 1, 1 )) (Assoc.findFirst greaterThanZero [ ( 1, 1 ) ])
        ]


getFirst : Test
getFirst =
    describe "getFirst"
        [ test "should be Nothing on empty lists" <|
            \_ -> Expect.equal Nothing (Assoc.getFirst 0 [])
        , test "should be Nothing if key does not exist" <|
            \_ -> Expect.equal Nothing (Assoc.getFirst 0 [ ( -2, 2 ), ( -1, 1 ) ])
        , test "should be Just value when key is present" <|
            \_ -> Expect.equal (Just 1) (Assoc.getFirst 1 [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ])
        , test "should fetch the first entry that matches" <|
            \_ -> Expect.equal (Just 1) (Assoc.getFirst 1 [ ( 1, 1 ), ( 1, 2 ) ])
        , test "should work on a list with a single pair" <|
            \_ -> Expect.equal (Just 1) (Assoc.getFirst 1 [ ( 1, 1 ) ])
        ]


getAll : Test
getAll =
    describe "getAll"
        [ test "should always return empty list on empty lists" <|
            \_ -> Expect.equal [] (Assoc.getAll 0 [])
        , test "should be empty list if key does not exist" <|
            \_ -> Expect.equal [] (Assoc.getAll 0 [ ( -2, 2 ), ( -1, 1 ) ])
        , test "should be fetch all values associated with the given key" <|
            \_ -> Expect.equal [ 1, 2 ] (Assoc.getAll 1 [ ( 0, 0 ), ( 1, 1 ), ( 1, 2 ) ])
        ]


removeFirstWith : Test
removeFirstWith =
    describe "removeFirstWith"
        [ test "should work on empty lists" <|
            \_ -> Expect.equal [] (Assoc.removeFirstWith greaterThanZero [])
        , test "should do nothing if key does not exist" <|
            \_ -> Expect.equal [ ( -2, 2 ), ( -1, 1 ) ] (Assoc.removeFirstWith greaterThanZero [ ( -2, 2 ), ( -1, 1 ) ])
        , test "should remove an entry when key matches" <|
            \_ -> Expect.equal [ ( 0, 0 ), ( 2, 2 ) ] (Assoc.removeFirstWith greaterThanZero [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ])
        , test "should only remove the first entry that matches" <|
            \_ -> Expect.equal [ ( 1, 2 ) ] (Assoc.removeFirstWith greaterThanZero [ ( 1, 1 ), ( 1, 2 ) ])
        , test "should work on a list with a single pair" <|
            \_ -> Expect.equal [] (Assoc.removeFirstWith greaterThanZero [ ( 1, 1 ) ])
        , test "should keep the list order" <|
            \_ -> Expect.equal [ ( -1, -1 ), ( 0, 0 ), ( 2, 2 ), ( 3, 3 ) ] (Assoc.removeFirstWith greaterThanZero [ ( -1, -1 ), ( 0, 0 ), ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ])
        ]


removeFirst : Test
removeFirst =
    describe "removeFirst"
        [ test "should work on empty lists" <|
            \_ -> Expect.equal [] (Assoc.removeFirst 0 [])
        , test "should do nothing if key does not exist" <|
            \_ -> Expect.equal [ ( 1, 1 ), ( 2, 2 ) ] (Assoc.removeFirst 0 [ ( 1, 1 ), ( 2, 2 ) ])
        , test "should remove an entry when key matches" <|
            \_ -> Expect.equal [ ( 0, 0 ), ( 2, 2 ) ] (Assoc.removeFirst 1 [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ])
        , test "should only remove the first entry that matches" <|
            \_ -> Expect.equal [ ( 1, 2 ) ] (Assoc.removeFirst 1 [ ( 1, 1 ), ( 1, 2 ) ])
        , test "should work on a list with a single pair" <|
            \_ -> Expect.equal [] (Assoc.removeFirst 1 [ ( 1, 1 ) ])
        , test "should keep the list order" <|
            \_ -> Expect.equal [ ( -1, -1 ), ( 0, 0 ), ( 2, 2 ), ( 3, 3 ) ] (Assoc.removeFirst 1 [ ( -1, -1 ), ( 0, 0 ), ( 1, 1 ), ( 2, 2 ), ( 3, 3 ) ])
        ]


removeAll : Test
removeAll =
    describe "removeAll"
        [ test "should work on empty lists" <|
            \_ -> Expect.equal [] (Assoc.removeAll 0 [])
        , test "should do nothing if key does not exist" <|
            \_ -> Expect.equal [ ( -2, 2 ), ( -1, 1 ) ] (Assoc.removeAll 0 [ ( -2, 2 ), ( -1, 1 ) ])
        , test "should remove all entries that match key" <|
            \_ -> Expect.equal [ ( 0, 0 ) ] (Assoc.removeAll 1 [ ( 0, 0 ), ( 1, 1 ), ( 1, 2 ) ])
        , test "should work on a list with a single pair" <|
            \_ -> Expect.equal [] (Assoc.removeAll 1 [ ( 1, 1 ) ])
        , test "should keep the list order" <|
            \_ -> Expect.equal [ ( -1, -1 ), ( 0, 0 ) ] (Assoc.removeAll 1 [ ( -1, -1 ), ( 0, 0 ), ( 1, 1 ), ( 1, 2 ), ( 1, 3 ) ])
        ]


filter : Test
filter =
    describe "filter"
        [ test "should always return empty list on empty lists" <|
            \_ -> Expect.equal [] (Assoc.filter greaterThanZero [])
        , test "should be empty list if pred is always false" <|
            \_ -> Expect.equal [] (Assoc.filter greaterThanZero [ ( -2, 2 ), ( -1, 1 ) ])
        , test "should fetch all tuples with pred true" <|
            \_ -> Expect.equal [ ( 1, 1 ), ( 2, 2 ) ] (Assoc.filter greaterThanZero [ ( 0, 0 ), ( 1, 1 ), ( 2, 2 ) ])
        , test "should keep the list order" <|
            \_ -> Expect.equal [ ( 1, 1 ), ( 2, 2 ) ] (Assoc.filter greaterThanZero [ ( -1, -1 ), ( 0, 0 ), ( 1, 1 ), ( 2, 2 ), ( -2, -2 ) ])
        ]


group : Test
group =
    describe "group"
        [ test "should work on empty lists" <|
            \_ -> Expect.equal [] (Assoc.group [])
        , test "should not group anything when keys are all different" <|
            \_ -> Expect.equal [ ( 1, [ 1 ] ), ( 2, [ 2 ] ) ] (Assoc.group [ ( 1, 1 ), ( 2, 2 ) ])
        , test "should group tuples with the same key" <|
            \_ -> Expect.equal [ ( 0, [ 0 ] ), ( 1, [ 1, 2 ] ) ] (Assoc.group [ ( 0, 0 ), ( 1, 1 ), ( 1, 2 ) ])
        , test "should be able to group items when they appear in list out of order" <|
            \_ ->
                Expect.equal
                    [ ( 0, [ 0, 1 ] ), ( 1, [ 2, 3 ] ) ]
                    (Assoc.group [ ( 1, 2 ), ( 0, 0 ), ( 1, 3 ), ( 0, 1 ) ])
        ]


groupBy : Test
groupBy =
    describe "groupBy"
        [ test "should work on empty lists" <|
            \_ -> Expect.equal [] (Assoc.groupBy identity [])
        , test "should not group anything when keys are all different" <|
            \_ -> Expect.equal [ ( -1, [ -1 ] ), ( 1, [ 1 ] ) ] (Assoc.groupBy sign [ ( -1, -1 ), ( 1, 1 ) ])
        , test "should group tuples with the same key" <|
            \_ -> Expect.equal [ ( 0, [ 0 ] ), ( 1, [ 1, 2 ] ) ] (Assoc.groupBy sign [ ( 0, 0 ), ( 1, 1 ), ( 1, 2 ) ])
        , test "should be able to group items when they appear in list out of order" <|
            \_ ->
                Expect.equal
                    [ ( -1, [ -1, -2 ] ), ( 0, [ 0, 1 ] ), ( 1, [ 2, 3 ] ) ]
                    (Assoc.groupBy sign [ ( 1, 2 ), ( 0, 0 ), ( -1, -1 ), ( 2, 3 ), ( 0, 1 ), ( -2, -2 ) ])
        ]


groupWith : Test
groupWith =
    describe "groupWith"
        [ test "should work on empty lists" <|
            \_ -> Expect.equal [] (Assoc.groupWith compare [])
        , test "should not group anything when keys are all different" <|
            \_ -> Expect.equal [ ( 1, [ 1 ] ), ( 2, [ 2 ] ) ] (Assoc.groupWith compare [ ( 1, 1 ), ( 2, 2 ) ])
        , test "should group tuples with the same key" <|
            \_ -> Expect.equal [ ( 0, [ 0 ] ), ( 1, [ 1, 2 ] ) ] (Assoc.groupWith compare [ ( 0, 0 ), ( 1, 1 ), ( 1, 2 ) ])
        , test "should be able to group items when they appear in list out of order" <|
            \_ ->
                Expect.equal
                    [ ( 0, [ 0, 1 ] ), ( 1, [ 2, 3 ] ) ]
                    (Assoc.groupWith compare [ ( 1, 2 ), ( 0, 0 ), ( 1, 3 ), ( 0, 1 ) ])
        ]
