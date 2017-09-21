module View exposing (view)

import Array exposing (Array)
import Css exposing (..)
import Css.Colors as Colors
import Css.Namespace exposing (namespace)
import Html exposing (..)
import Html.CssHelpers
import Html.Events exposing (onClick)
import Model exposing (..)
import Types exposing (..)
import Update exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ attachElmCssStyles
        , viewGrid model
        , viewRules model
        ]


viewRules : Model -> Html Msg
viewRules model =
    let
        rules =
            List.map
                (\rule ->
                    li []
                        [ Html.text (toString rule)
                        , button [ onClick (RemoveRule rule) ] [ Html.text "Remove Rule" ]
                        ]
                )
                model.rules
    in
    div []
        [ Html.text "Rules"
        , ul [] rules
        ]


viewGrid : Model -> Html Msg
viewGrid model =
    div [ class [ Grid ] ]
        (model.grid
            |> Array.toList
            |> List.indexedMap viewRow
        )


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

        OnFire ->
            FireCell


type CssClass
    = Grid
    | Row
    | CellClass
    | EmptyCell
    | MossyCell
    | TreeCell
    | FireCell


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
        , Css.class FireCell
            [ backgroundColor Colors.red
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
