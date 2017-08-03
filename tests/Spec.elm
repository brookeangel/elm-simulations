module Spec exposing (..)

import ApplyRules exposing (applyRules)
import Array
import Expect
import Test exposing (..)
import Types exposing (..)


spec : Test
spec =
    describe "Applying rules"
        [ describe "ChangeFromAToB"
            [ test "does not change cells that are not type A" <|
                \() ->
                    let
                        grid =
                            gridFromLists [ [ Empty ] ]
                    in
                    grid
                        |> applyRules [ ChangeFromAToB FullOfMoss FullOfTrees ]
                        |> Expect.equal grid
            , test "changes cells of type A to type B with one cell" <|
                \() ->
                    let
                        grid =
                            gridFromLists [ [ Empty ] ]
                    in
                    grid
                        |> applyRules [ ChangeFromAToB Empty FullOfMoss ]
                        |> Expect.equal (gridFromLists [ [ FullOfMoss ] ])
            , test "applies correctly when there are multiple cells" <|
                \() ->
                    let
                        grid =
                            gridFromLists
                                [ [ Empty, FullOfMoss ]
                                , [ FullOfMoss, FullOfTrees ]
                                ]
                    in
                    grid
                        |> applyRules [ ChangeFromAToB Empty FullOfTrees ]
                        |> Expect.equal
                            (gridFromLists
                                [ [ FullOfTrees, FullOfMoss ]
                                , [ FullOfMoss, FullOfTrees ]
                                ]
                            )
            ]
        , describe "Sequences of rules"
            [ todo "Rules are applied based on the initial grid"
            , todo "In rules [A, B], rule B is applied second"
            ]
        ]


gridFromLists : List (List CellState) -> Grid
gridFromLists cells =
    cells
        |> List.map Array.fromList
        |> Array.fromList
