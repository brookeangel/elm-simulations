module Spec exposing (..)

import Array
import EveryDict
import Expect
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
                        |> applyRules EveryDict.empty [ ChangeToB FullOfMoss ]
                        |> Expect.equal (gridFromLists [ [ FullOfMoss ] ])
            , test "applies correctly when there are multiple cells" <|
                \() ->
                    gridFromLists
                        [ [ Empty, FullOfMoss ]
                        , [ FullOfMoss, FullOfTrees ]
                        ]
                        |> applyRules EveryDict.empty [ ChangeToB FullOfTrees ]
                        |> Expect.equal
                            (gridFromLists
                                [ [ FullOfTrees, FullOfTrees ]
                                , [ FullOfTrees, FullOfTrees ]
                                ]
                            )
            ]
        , describe "ProbabilityGrid"
            [ todo "correctly determines whether to apply rules"
            ]
        , describe "IfCellIs"
            [ test "applies the rule when the cell matches the conditional" <|
                \() ->
                    gridFromLists
                        [ [ Empty, FullOfMoss ]
                        , [ FullOfMoss, FullOfTrees ]
                        ]
                        |> applyRules EveryDict.empty [ IfCellIs FullOfMoss (ChangeToB FullOfTrees) ]
                        |> Expect.equal
                            (gridFromLists
                                [ [ Empty, FullOfTrees ]
                                , [ FullOfTrees, FullOfTrees ]
                                ]
                            )
            ]
        , describe "Sequences of rules"
            [ test "Once a rule has been applied to a cell, stop changing it" <|
                \() ->
                    gridFromLists [ [ Empty ] ]
                        |> applyRules EveryDict.empty
                            [ ChangeToB FullOfTrees
                            , ChangeToB FullOfMoss
                            ]
                        |> Expect.equal
                            (gridFromLists [ [ FullOfTrees ] ])
            , test "Works when the first rule does not apply" <|
                \() ->
                    gridFromLists [ [ Empty ] ]
                        |> applyRules EveryDict.empty
                            [ IfCellIs FullOfMoss (ChangeToB FullOfTrees)
                            , ChangeToB FullOfTrees
                            ]
                        |> Expect.equal (gridFromLists [ [ FullOfTrees ] ])
            ]
        ]


gridFromLists : List (List CellState) -> Grid
gridFromLists cells =
    cells
        |> List.map (List.map (Cell False))
        |> List.map Array.fromList
        |> Array.fromList
