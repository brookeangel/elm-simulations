module Main exposing (..)

import Array exposing (Array)
import Css exposing (..)
import Css.Colors as Colors
import Css.Namespace exposing (namespace)
import Html exposing (..)
import Html.CssHelpers
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
    { grid = initGrid
    , rules =
        [ ChangeFromAToB Empty FullOfMoss
        , ChangeFromAToB FullOfMoss FullOfTrees
        , ChangeFromAToB FullOfTrees Empty
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


viewRow : Int -> Array Cell -> Html Msg
viewRow rowIndex row =
    row
        |> Array.toList
        |> List.indexedMap (viewCell rowIndex)
        |> div [ class [ Row ] ]


viewCell : Int -> Int -> Cell -> Html Msg
viewCell rowIndex columnIndex cell =
    div [ class [ CellClass, cellStateToClass cell.state ] ] []


cellStateToClass : CellState -> CssClass
cellStateToClass cellState =
    case cellState of
        Empty ->
            EmptyCell

        FullOfMoss ->
            MossyCell

        FullOfTrees ->
            TreeCell


type CssClass
    = Grid
    | Row
    | CellClass
    | EmptyCell
    | MossyCell
    | TreeCell


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
        , Css.class CellClass
            [ border3 (px 1) solid Colors.blue
            , width (px 20)
            , height (px 20)
            ]
        , Css.class MossyCell
            [ backgroundColor Colors.green ]
        , Css.class TreeCell
            [ backgroundColor Colors.olive
            ]
        ]


cssNamespace : String
cssNamespace =
    "simulations"


{ class, classList } =
    Html.CssHelpers.withNamespace cssNamespace


attachElmCssStyles : Html Msg
attachElmCssStyles =
    Html.CssHelpers.style <| .css <| Css.compile [ styles ]
