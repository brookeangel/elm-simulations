module Main exposing (..)

import Array exposing (Array)
import Css exposing (..)
import Css.Colors as Colors
import Css.Namespace exposing (namespace)
import Html exposing (..)
import Html.CssHelpers
import Html.Events exposing (..)
import Model exposing (..)
import Platform.Sub
import Rocket exposing (..)
import Time
import Types exposing (..)
import Update exposing (..)


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
    { grid = Array.repeat defaultSize (Array.repeat defaultSize Empty)
    , rules =
        [ ChangeFromAToB Empty FullOfMoss
        , ChangeFromAToB FullOfMoss Empty
        ]
    }
        => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second (\_ -> NextFrame)


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
