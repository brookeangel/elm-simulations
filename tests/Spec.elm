module Spec exposing (..)

import Array
import Expect
import Fuzz
import Random exposing (Seed)
import Rules exposing (..)
import Test exposing (..)
import Types exposing (..)


spec : Test
spec =
    describe "Applying rules"
        [ describe "ChangeToB"
            [ test "changes cells to type B with one cell" <|
                \() ->
                    gridFromLists [ [ Empty ] ]
                        |> applyRules seed [ ChangeToB FullOfMoss ]
                        |> Tuple.first
                        |> Expect.equal (gridFromLists [ [ FullOfMoss ] ])
            , test "applies correctly when there are multiple cells" <|
                \() ->
                    gridFromLists
                        [ [ Empty, FullOfMoss ]
                        , [ FullOfMoss, FullOfTrees ]
                        ]
                        |> applyRules seed [ ChangeToB FullOfTrees ]
                        |> Tuple.first
                        |> Expect.equal
                            (gridFromLists
                                [ [ FullOfTrees, FullOfTrees ]
                                , [ FullOfTrees, FullOfTrees ]
                                ]
                            )
            ]
        , describe "Probability"
            [ fuzz Fuzz.float "updates the seed" <|
                \roll ->
                    gridFromLists
                        [ [ Empty, FullOfMoss ]
                        , [ FullOfMoss, FullOfTrees ]
                        ]
                        |> applyRules seed [ Probability roll (ChangeToB FullOfTrees) ]
                        |> Tuple.second
                        |> Expect.notEqual seed
            , todo "applies when the probability dictates"
            ]
        , describe "IfCellIs"
            [ test "applies the rule when the cell matches the conditional" <|
                \() ->
                    gridFromLists
                        [ [ Empty, FullOfMoss ]
                        , [ FullOfMoss, FullOfTrees ]
                        ]
                        |> applyRules seed [ IfCellIs FullOfMoss (ChangeToB FullOfTrees) ]
                        |> Tuple.first
                        |> Expect.equal
                            (gridFromLists
                                [ [ Empty, FullOfTrees ]
                                , [ FullOfTrees, FullOfTrees ]
                                ]
                            )
            ]
        , describe "IfXNeighborsAre"
            [ test "does not apply the rule when less than x neighbors match" <|
                \() ->
                    gridFromLists
                        [ [ Empty, Empty, Empty ]
                        , [ Empty, FullOfTrees, Empty ]
                        , [ FullOfMoss, FullOfMoss, Empty ]
                        ]
                        |> applyRules seed
                            [ IfCellIs FullOfTrees
                                (IfXNeighborsAre
                                    3
                                    FullOfMoss
                                    (ChangeToB FullOfMoss)
                                )
                            ]
                        |> Tuple.first
                        |> Expect.equal
                            (gridFromLists
                                [ [ Empty, Empty, Empty ]
                                , [ Empty, FullOfTrees, Empty ]
                                , [ FullOfMoss, FullOfMoss, Empty ]
                                ]
                            )
            , test "applies if x neighbors match" <|
                \() ->
                    gridFromLists
                        [ [ Empty, Empty, FullOfMoss ]
                        , [ Empty, FullOfTrees, Empty ]
                        , [ FullOfMoss, FullOfMoss, Empty ]
                        ]
                        |> applyRules seed
                            [ IfCellIs FullOfTrees
                                (IfXNeighborsAre
                                    3
                                    FullOfMoss
                                    (ChangeToB FullOfMoss)
                                )
                            ]
                        |> Tuple.first
                        |> Expect.equal
                            (gridFromLists
                                [ [ Empty, Empty, FullOfMoss ]
                                , [ Empty, FullOfMoss, Empty ]
                                , [ FullOfMoss, FullOfMoss, Empty ]
                                ]
                            )
            , test "applies if > x neighbors match" <|
                \() ->
                    gridFromLists
                        [ [ FullOfMoss, Empty, FullOfMoss ]
                        , [ Empty, FullOfTrees, Empty ]
                        , [ FullOfMoss, FullOfMoss, Empty ]
                        ]
                        |> applyRules seed
                            [ IfCellIs FullOfTrees
                                (IfXNeighborsAre
                                    3
                                    FullOfMoss
                                    (ChangeToB FullOfMoss)
                                )
                            ]
                        |> Tuple.first
                        |> Expect.equal
                            (gridFromLists
                                [ [ FullOfMoss, Empty, FullOfMoss ]
                                , [ Empty, FullOfMoss, Empty ]
                                , [ FullOfMoss, FullOfMoss, Empty ]
                                ]
                            )
            ]
        , describe "Sequences of rules"
            [ test "Once a rule has been applied to a cell, stop changing it" <|
                \() ->
                    gridFromLists [ [ Empty ] ]
                        |> applyRules seed
                            [ ChangeToB FullOfTrees
                            , ChangeToB FullOfMoss
                            ]
                        |> Tuple.first
                        |> Expect.equal (gridFromLists [ [ FullOfTrees ] ])
            , test "Works when the first rule does not apply" <|
                \() ->
                    gridFromLists [ [ Empty ] ]
                        |> applyRules seed
                            [ IfCellIs FullOfMoss (ChangeToB FullOfTrees)
                            , ChangeToB FullOfTrees
                            ]
                        |> Tuple.first
                        |> Expect.equal (gridFromLists [ [ FullOfTrees ] ])
            ]
        ]


seed : Seed
seed =
    Random.initialSeed 424242


gridFromLists : List (List CellState) -> Grid
gridFromLists cells =
    cells
        |> List.map (List.map (Cell False))
        |> List.map Array.fromList
        |> Array.fromList
