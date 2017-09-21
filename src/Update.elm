module Update exposing (Msg(..), update)

import List.Extra
import Model exposing (..)
import Random exposing (Seed)
import Rocket exposing (..)
import Rules exposing (..)


type Msg
    = NextFrame
    | SetSeed Seed
    | RemoveRule Rule


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

        RemoveRule rule ->
            { model | rules = List.Extra.remove rule model.rules } => Cmd.none
