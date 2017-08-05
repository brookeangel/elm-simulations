module Update exposing (Msg(..), update)

import ApplyRules exposing (..)
import Model exposing (..)
import Rocket exposing (..)


type Msg
    = NextFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextFrame ->
            { model | grid = applyRules model.rules model.grid } => Cmd.none
