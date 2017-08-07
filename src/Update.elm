module Update exposing (Msg(..), update)

import Model exposing (..)
import ProbabilityGrid exposing (..)
import Random
import Rocket exposing (..)
import Rules exposing (..)


type Msg
    = NextFrame
    | SetProbabilityGrid (ProbabilityRuleGrids Rule)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextFrame ->
            let
                probabilityGridCmd =
                    Random.generate SetProbabilityGrid (probabilityGridGeneratorForRules model.rules)
            in
            { model | grid = applyRules model.probabilityGrid model.rules model.grid } => probabilityGridCmd

        SetProbabilityGrid probabilityGrid ->
            { model | probabilityGrid = probabilityGrid } => Cmd.none
