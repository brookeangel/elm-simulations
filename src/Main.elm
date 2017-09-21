module Main exposing (..)

import Html exposing (program)
import Model exposing (..)
import Random
import Rocket exposing (..)
import Rules exposing (..)
import Time
import Types exposing (..)
import Update exposing (..)
import View exposing (view)


{-|

    TODO:
    - currently, rules apply in the order of priority & always look at the "new" grid - should they do this? Maybe ok.
    - Add rule: If cell is not...
    - Add interface for adding rules
    - Add interface for adding items
    - Make actual design
    - Test probability rule
    - Make embeddable

-}
main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : ( Model, Cmd Msg )
init =
    { grid = initGrid
    , rules =
        [ IfCellIs FullOfTrees (IfXNeighborsAre 1 OnFire (ChangeToB OnFire))
        , IfCellIs FullOfMoss (IfXNeighborsAre 1 OnFire (ChangeToB OnFire))
        , IfCellIs Empty (Probability 0.05 (ChangeToB FullOfMoss))
        , IfXNeighborsAre 3 FullOfMoss (ChangeToB FullOfTrees)
        , Probability 0.001 (ChangeToB OnFire)
        , IfCellIs OnFire (ChangeToB Empty)
        ]
    , seed = Random.initialSeed 2810 -- TODO
    }
        => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (100 * Time.millisecond) (\_ -> NextFrame)
