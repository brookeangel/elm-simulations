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
import Time


main : Program Never Model Msg
main =
    program
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { grid : Grid
    , rules : List Rule
    }


type alias Grid =
    Array (Array CellState)


type CellState
    = Empty
    | FullOfMoss


type Rule
    = ChangeFromAToB CellState CellState


init : ( Model, Cmd Msg )
init =
    { grid = Array.repeat defaultSize (Array.repeat defaultSize Empty)
    , rules =
        [ ChangeFromAToB Empty FullOfMoss
        , ChangeFromAToB FullOfMoss Empty
        ]
    }
        => Cmd.none


defaultSize : Int
defaultSize =
    50


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every Time.second (\_ -> NextFrame)


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


applyRules : List Rule -> Grid -> Grid
applyRules rules grid =
    List.foldl applyRule grid rules


applyRule : Rule -> Grid -> Grid
applyRule rule grid =
    let
        applyToCell cell =
            case rule of
                ChangeFromAToB x y ->
                    if cell == x then
                        y
                    else
                        cell
    in
    Array.map (Array.map applyToCell) grid


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
