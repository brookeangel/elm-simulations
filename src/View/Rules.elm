module View.Rules exposing (viewAddRule, viewRules)

import Dict
import Html exposing (..)
import Html.Attributes
import Html.Events exposing (..)
import Json.Decode exposing (Decoder, andThen, decodeValue)
import Model exposing (..)
import Types exposing (..)
import Update exposing (..)


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
    div []
        [ viewChangeToBRule model
        ]


select : a -> List a -> (String -> Decoder a) -> Html a
select selected options decoder =
    let
        typeOptions =
            List.map
                (\value ->
                    option
                        [ Html.Attributes.selected (value == selected)
                        ]
                        [ Html.text (toString value)
                        ]
                )
                options

        onSelectHandler =
            on "change" (targetValue |> andThen decoder)
    in
    Html.select [] typeOptions


viewChangeToBRule : Model -> Html Msg
viewChangeToBRule model =
    let
        valueLookup =
            allCellStates
                |> List.map (\x -> ( toString x, x ))
                |> Dict.fromList

        decodeValue string =
            Dict.get string valueLookup
                |> Maybe.map Json.Decode.succeed
                |> Maybe.withDefault (Json.Decode.fail "Could not decode")
    in
    div []
        [ h2 [] [ Html.text "Add ChangeToB rule" ]
        , Html.map ChangeCellState (select model.selectedCellState allCellStates decodeValue)
        , button [ onClick AddChangeToBRule ] [ Html.text "Add" ]
        ]


viewProbabilityRule : Model -> Html Msg
viewProbabilityRule model =
    let
        valueLookup =
            allCellStates
                |> List.map (\x -> ( toString x, x ))
                |> Dict.fromList

        decodeValue string =
            Dict.get string valueLookup
                |> Maybe.map Json.Decode.succeed
                |> Maybe.withDefault (Json.Decode.fail "Could not decode")
    in
    div []
        [ h2 [] [ Html.text "Add ChangeToB rule" ]
        , Html.map ChangeCellState (select model.selectedCellState allCellStates decodeValue)
        , button [ onClick AddChangeToBRule ] [ Html.text "Add" ]
        ]
