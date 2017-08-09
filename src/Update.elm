module Update exposing (Msg(..), update)

import Model exposing (..)
import Random exposing (Seed)
import Rocket exposing (..)
import Rules exposing (..)


type Msg
    = NextFrame
    | SetSeed Seed


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextFrame ->
            let
                ( grid, seed ) =
                    applyRules model.seed model.rules model.grid
            in
            { model | grid = grid, seed = seed } => Cmd.none

        SetSeed seed ->
            { model | seed = seed } => Cmd.none
