module View exposing (view)

import Array exposing (Array)
import Html exposing (..)
import Model exposing (..)
import Styles exposing (..)
import Types exposing (..)
import Update exposing (..)
import View.Rules exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ attachElmCssStyles
        , viewGrid model
        , viewRules model
        , viewAddRule model
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
