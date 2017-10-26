module Styles exposing (..)

import Css exposing (..)
import Css.Colors as Colors
import Css.Namespace exposing (namespace)
import Html exposing (..)
import Html.CssHelpers
import Types exposing (..)


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


attachElmCssStyles : Html msg
attachElmCssStyles =
    Html.CssHelpers.style <| .css <| Css.compile [ styles ]
