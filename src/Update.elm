module Update exposing (Msg(..), update)

import Model exposing (..)
import Rocket exposing (..)
import Rules exposing (..)
import Types exposing (..)


type Msg
    = NextFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NextFrame ->
            { model | grid = applyRules model.rules model.grid } => Cmd.none
