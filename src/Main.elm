module Main exposing (..)

import Array exposing (Array)
import Css exposing (..)
import Css.Colors as Colors
import Css.Namespace exposing (namespace)
import Html exposing (..)
import Html.CssHelpers
import Html.Events exposing (..)
import Platform.Sub
import Rocket exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }


type alias Model =
    { grid : Array (Array CellState) }


type CellState
    = Empty
    | FullOfMoss


init : ( Model, Cmd Msg )
init =
    ( { grid = Array.repeat 50 (Array.repeat 50 Empty) }
    , Cmd.none
    )


type Msg
    = GenerateMoss Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GenerateMoss clickedRow clickedColumn ->
            let
                newGrid =
                    model.grid
                        |> Array.set clickedRow
                            (model.grid
                                |> Array.get clickedRow
                                |> withDefaultLazy (\() -> Array.repeat 50 Empty)
                                |> Array.set clickedColumn FullOfMoss
                            )
            in
            { model | grid = newGrid } => Cmd.none


withDefaultLazy : (() -> a) -> Maybe a -> a
withDefaultLazy default maybe =
    -- TODO: er... does this already exist? I suspect yes...
    case maybe of
        Just a ->
            a

        Nothing ->
            default ()


view : Model -> Html Msg
view model =
    div []
        [ attachElmCssStyles
        , div [ class [ Grid ] ]
            (model.grid
                |> Array.toList
                |> List.indexedMap viewRow
            )
        ]


viewRow : Int -> Array CellState -> Html Msg
viewRow rowIndex row =
    row
        |> Array.toList
        |> List.indexedMap (viewCell rowIndex)
        |> div [ class [ Row ] ]


viewCell : Int -> Int -> CellState -> Html Msg
viewCell rowIndex columnIndex cell =
    div
        [ class [ Cell, cellStateToClass cell ]
        , onClick (GenerateMoss rowIndex columnIndex)
        ]
        []


cellStateToClass : CellState -> CssClass
cellStateToClass cellState =
    case cellState of
        Empty ->
            EmptyCell

        FullOfMoss ->
            MossyCell


type CssClass
    = Grid
    | Row
    | Cell
    | EmptyCell
    | MossyCell


styles : Css.Stylesheet
styles =
    (stylesheet << namespace cssNamespace)
        [ Css.class Grid
            [ displayFlex
            , flexDirection column
            ]
        , Css.class Row
            [ displayFlex
            ]
        , Css.class Cell
            [ border3 (px 1) solid Colors.blue
            , width (px 20)
            , height (px 20)
            ]
        , Css.class MossyCell
            [ backgroundColor Colors.green ]
        ]


cssNamespace : String
cssNamespace =
    "simulations"


{ class, classList } =
    Html.CssHelpers.withNamespace cssNamespace


attachElmCssStyles : Html Msg
attachElmCssStyles =
    Html.CssHelpers.style <| .css <| Css.compile [ styles ]
