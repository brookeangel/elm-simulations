module Main exposing (..)

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
    { grid : List (List CellState) }


type CellState
    = Empty
    | FullOfMoss


init : ( Model, Cmd Msg )
init =
    ( { grid = List.repeat 50 (List.repeat 50 Empty) }
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
                        |> List.indexedMap
                            (\rowIndex row ->
                                List.indexedMap
                                    (\columnIndex cell ->
                                        if columnIndex == clickedColumn && rowIndex == clickedRow then
                                            FullOfMoss
                                        else
                                            cell
                                    )
                                    row
                            )
            in
            { model | grid = newGrid } => Cmd.none


view : Model -> Html Msg
view model =
    div []
        [ attachElmCssStyles
        , div [ class [ Grid ] ]
            (model.grid
                |> List.indexedMap
                    (\rowIndex row ->
                        row
                            |> List.indexedMap
                                (\columnIndex cell ->
                                    div
                                        [ class [ Cell, cellStateToClass cell ]
                                        , onClick (GenerateMoss rowIndex columnIndex)
                                        ]
                                        []
                                )
                            |> div [ class [ Row ] ]
                    )
            )
        ]


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
