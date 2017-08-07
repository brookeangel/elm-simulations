module Spec exposing (..)

import Array
import Expect
import Rules exposing (..)
import Test exposing (..)
import Types exposing (..)


spec : Test
spec =
    describe "Applying rules"
        [ describe "ChangeFromAToB"
            [ test "does not change cells that are not type A" <|
                \() ->
                    gridFromLists [ [ Empty ] ]
                        |> applyRules [ Simple <| ChangeFromAToB FullOfMoss FullOfTrees ]
                        |> Expect.equal (gridFromLists [ [ Empty ] ])
            , test "changes cells of type A to type B with one cell" <|
                \() ->
                    gridFromLists [ [ Empty ] ]
                        |> applyRules [ Simple <| ChangeFromAToB Empty FullOfMoss ]
                        |> Expect.equal (gridFromLists [ [ FullOfMoss ] ])
            , test "applies correctly when there are multiple cells" <|
                \() ->
                    gridFromLists
                        [ [ Empty, FullOfMoss ]
                        , [ FullOfMoss, FullOfTrees ]
                        ]
                        |> applyRules [ Simple <| ChangeFromAToB Empty FullOfTrees ]
                        |> Expect.equal
                            (gridFromLists
                                [ [ FullOfTrees, FullOfMoss ]
                                , [ FullOfMoss, FullOfTrees ]
                                ]
                            )
            ]
        , describe "ProbabilityGrid"
            [ todo "only creates grids for probability rules"
            , todo "correctly determines whether to apply rules"
            ]
        , describe "Sequences of rules"
            [ test "Once a rule has been applied to a cell, stop changing it" <|
                \() ->
                    gridFromLists [ [ Empty ] ]
                        |> applyRules
                            [ Simple <| ChangeFromAToB Empty FullOfTrees
                            , Simple <| ChangeFromAToB FullOfTrees FullOfMoss
                            ]
                        |> Expect.equal
                            (gridFromLists [ [ FullOfTrees ] ])
            , test "Works when the first rule does not apply" <|
                \() ->
                    gridFromLists [ [ Empty ] ]
                        |> applyRules
                            [ Simple <| ChangeFromAToB FullOfTrees FullOfMoss
                            , Simple <| ChangeFromAToB Empty FullOfTrees
                            , Simple <| ChangeFromAToB Empty FullOfMoss
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
