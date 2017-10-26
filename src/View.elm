module View exposing (view)

import Array exposing (Array)
import Dict
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (..)
import Json.Decode exposing (andThen, decodeValue)
import Model exposing (..)
import Styles exposing (..)
import Types exposing (..)
import Update exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ attachElmCssStyles
        , viewGrid model
        , viewRules model
        , viewAddRule model
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
        [ h2 [] [ Html.text "Rules" ]
        , ul [] rules
        ]


viewAddRule : Model -> Html Msg
viewAddRule model =
    let
        typeOptions =
            List.map
                (\cellState ->
                    option
                        [ Html.Attributes.selected (model.selectedCellState == cellState)
                        , onClick cellState
                        ]
                        [ Html.text (toString cellState)
                        ]
                )
                allCellStates

        valueLookup =
            allCellStates
                |> List.map (\x -> ( toString x, x ))
                |> Dict.fromList

        decodeValue string =
            Dict.get string valueLookup
                |> Maybe.map Json.Decode.succeed
                |> Maybe.withDefault (Json.Decode.fail "Could not decode")

        onSelectHandler =
            on "change" (targetValue |> andThen decodeValue)
    in
    div []
        [ h2 [] [ Html.text "Add ChangeToB rule" ]
        , Html.map ChangeCellState (select [ onSelectHandler ] typeOptions)
        , button [ onClick AddChangeToBRule ] [ Html.text "Add" ]
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
