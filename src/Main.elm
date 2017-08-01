module Main exposing (..)

import Css exposing (..)
import Css.Colors as Colors
import Css.Namespace exposing (namespace)
import Html exposing (..)
import Html.CssHelpers
import Platform.Sub


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


init : ( Model, Cmd Msg )
init =
    ( { grid = List.repeat 100 (List.repeat 100 Empty) }
    , Cmd.none
    )


type CellState
    = Empty


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ attachElmCssStyles
        , div [ class [ Grid ] ]
            (model.grid
                |> List.map
                    (\row ->
                        row
                            |> List.map
                                (\cell ->
                                    div [ class [ Cell, cellStateToClass cell ] ]
                                        []
                                )
                            |> div [ class [ Row ] ]
                    )
            )
        ]


type Msg
    = NoOp


cellStateToClass : CellState -> CssClass
cellStateToClass cellState =
    case cellState of
        Empty ->
            EmptyCell


type CssClass
    = Grid
    | Row
    | Cell
    | EmptyCell


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
        ]


cssNamespace : String
cssNamespace =
    "simulations"


{ class, classList } =
    Html.CssHelpers.withNamespace cssNamespace


attachElmCssStyles : Html Msg
attachElmCssStyles =
    Html.CssHelpers.style <| .css <| Css.compile [ styles ]
