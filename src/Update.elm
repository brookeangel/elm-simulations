module Update exposing (Msg(..), update)

import ApplyRules exposing (..)
import Array exposing (..)
import Model exposing (..)
import Rocket exposing (..)
import Types exposing (..)


type Msg
    = GenerateMoss Int Int
    | NextFrame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateMoss clickedRow clickedColumn ->
            { model | grid = updateGridAt clickedRow clickedColumn FullOfMoss model.grid }
                => Cmd.none

        NextFrame ->
            { model | grid = applyRules model.rules model.grid } => Cmd.none


updateGridAt : Int -> Int -> CellState -> Grid -> Grid
updateGridAt row column cellState grid =
    grid
        |> Array.set row
            (grid
                |> Array.get row
                |> withDefaultLazy (\() -> Array.repeat defaultSize Empty)
                |> Array.set column FullOfMoss
            )


withDefaultLazy : (() -> a) -> Maybe a -> a
withDefaultLazy default maybe =
    -- TODO: er... does this already exist? I suspect yes...
    case maybe of
        Just a ->
            a

        Nothing ->
            default ()
