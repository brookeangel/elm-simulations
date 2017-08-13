module Main exposing (..)

import Array exposing (Array)
import Css exposing (..)
import Css.Colors as Colors
import Css.Namespace exposing (namespace)
import Html exposing (..)
import Html.CssHelpers
import Model exposing (..)
import Platform.Sub
import Random
import Rocket exposing (..)
import Rules exposing (..)
import Time
import Types exposing (..)
import Update exposing (..)


{-|

    TODO:
    - currently, rules apply in the order of priority & always look at the "new" grid - should they do this? Maybe ok.
    - Add rule: If cell is not...
    - Add interface for adding rules
    - Add interface for adding items
    - Make actual design
    - Test probability rule
    - Make embeddable

-}
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
        [ IfCellIs FullOfTrees (IfXNeighborsAre 1 OnFire (ChangeToB OnFire))
        , IfCellIs FullOfMoss (IfXNeighborsAre 1 OnFire (ChangeToB OnFire))
        , IfCellIs Empty (Probability 0.05 (ChangeToB FullOfMoss))
        , IfXNeighborsAre 3 FullOfMoss (ChangeToB FullOfTrees)
        , Probability 0.001 (ChangeToB OnFire)
        , IfCellIs OnFire (ChangeToB Empty)
        ]
    , seed = Random.initialSeed 2810 -- TODO
    }
        => Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (100 * Time.millisecond) (\_ -> NextFrame)


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
