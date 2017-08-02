module Main exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)


spec : Test
spec =
    describe "Applying rules"
        [ describe "ChangeFromAToB"
            [ todo "does not change cells that are not type A"
            , todo "changes cells of type A to type B"
            ]
        , describe "Sequences of rules"
            [ todo "Rules are applied based on the initial grid"
            , todo "In rules [A, B], rule B is applied second"
            ]
        ]
