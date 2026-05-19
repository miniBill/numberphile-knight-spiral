module Main exposing (main)

import Array exposing (Array)
import Browser
import Color exposing (Color)
import FastDict as Dict exposing (Dict)
import FastSet as Set exposing (Set)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import TypedSvg as S
import TypedSvg.Attributes as SA
import TypedSvg.Attributes.InPx
import TypedSvg.Core as S exposing (Svg)
import TypedSvg.Types exposing (AlignmentBaseline(..), AnchorAlignment(..), DominantBaseline(..), Paint(..))


type alias Model =
    { pieces : List Piece
    , size : Int
    , history : List Board
    }


type alias Piece =
    { name : String
    , color : Color
    , moves : List ( Int, Int )
    }


type alias Board =
    { cells : Array Cell
    , openList : Dict Int ( Int, Int, Maybe Color )
    }


type Cell
    = Open
    | Colored Color
    | Controlled Color
    | Unusable


type Msg
    = Size Int


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


init : Model
init =
    let
        pieces : List Piece
        pieces =
            [ knight (Color.rgb 1 0.6 0.6)
            , knight (Color.rgb 0.5 0.5 0.5)
            ]
    in
    { pieces = pieces
    , size = 7
    , history = compute 7 pieces
    }


knight : Color -> Piece
knight color =
    { name = "Knight"
    , color = color
    , moves = [ ( 2, 1 ), ( 1, 2 ), ( 2, -1 ), ( -1, 2 ), ( -2, 1 ), ( 1, -2 ), ( -2, -1 ), ( -1, -2 ) ]
    }


view : Model -> Html Msg
view model =
    Html.main_
        [ Html.Attributes.style "padding" "8px"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "gap" "8px"
        ]
        [ model.pieces
            |> List.map
                (\piece ->
                    Html.li []
                        [ Html.div
                            [ Html.Attributes.style "width" "16px"
                            , Html.Attributes.style "height" "16px"
                            , Html.Attributes.style "display" "inline-block"
                            , Html.Attributes.style "background" (Color.toCssString piece.color)
                            ]
                            []
                        , Html.text ("  " ++ piece.name ++ " ")
                        , Html.text (Debug.toString piece.moves)
                        ]
                )
            |> Html.ul []
        , Html.div []
            [ Html.input
                [ Html.Attributes.type_ "number"
                , Html.Attributes.value (String.fromInt model.size)
                , Html.Events.onInput (\v -> v |> String.toInt |> Maybe.withDefault model.size |> Size)
                ]
                []
            ]
        , model.history
            |> List.map (\board -> Html.li [] [ viewBoard board ])
            |> Html.ul []
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    let
        edge : Int
        edge =
            board.cells
                |> Array.length
                |> toFloat
                |> sqrt
                |> ceiling

        halfEdge : Int
        halfEdge =
            edge // 2
    in
    (viewBoardCells board
        ++ viewOpenList board
    )
        |> S.svg
            [ Html.Attributes.style "width" "50%"
            , Html.Attributes.style "margin" "auto"
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "font-size" "0.2px"
            , TypedSvg.Attributes.InPx.strokeWidth 0.01
            , SA.viewBox -(toFloat halfEdge + 0.5) -(toFloat halfEdge + 0.5) (toFloat edge) (toFloat edge)
            ]


viewOpenList : Board -> List (Svg Msg)
viewOpenList board =
    board.openList
        |> Dict.toList
        |> List.map
            (\( _, ( x, y, mc ) ) ->
                S.circle
                    [ TypedSvg.Attributes.InPx.cx (toFloat x)
                    , TypedSvg.Attributes.InPx.cy (toFloat y)
                    , TypedSvg.Attributes.InPx.r 0.1
                    , SA.fill (Paint (mc |> Maybe.withDefault Color.gray))
                    ]
                    []
            )


viewBoardCells : Board -> List (Svg msg)
viewBoardCells board =
    let
        edge : Int
        edge =
            board.cells
                |> Array.length
                |> toFloat
                |> sqrt
                |> ceiling

        halfEdge : Int
        halfEdge =
            edge // 2
    in
    allCells edge <|
        \x y ->
            let
                s : Int
                s =
                    toSpiral x y

                color : Color
                color =
                    case Array.get s board.cells of
                        Nothing ->
                            Color.red

                        Just Open ->
                            Color.white

                        Just (Colored c) ->
                            c

                        Just (Controlled c) ->
                            c

                        Just Unusable ->
                            Color.gray
            in
            viewCell s x y color


viewCell : Int -> Int -> Int -> Color -> Svg msg
viewCell s x y color =
    S.g []
        [ S.rect
            [ TypedSvg.Attributes.InPx.x (toFloat x - 0.5)
            , TypedSvg.Attributes.InPx.y (toFloat y - 0.5)
            , TypedSvg.Attributes.InPx.width 1
            , TypedSvg.Attributes.InPx.height 1
            , SA.fill (Paint color)
            ]
            []
        , S.text_
            [ TypedSvg.Attributes.InPx.x (toFloat x)
            , TypedSvg.Attributes.InPx.y (toFloat y)
            , SA.textAnchor AnchorMiddle
            , SA.dominantBaseline DominantBaselineMiddle
            ]
            [ "({x}, {y}) {s}"
                |> String.replace "{x}" (String.fromInt x)
                |> String.replace "{y}" (String.fromInt y)
                |> String.replace "{s}" (String.fromInt s)
                |> S.text
            ]
        ]


toSpiral : Int -> Int -> Int
toSpiral x y =
    -- Formula by Mitchell Spector, at https://math.stackexchange.com/a/1860731
    let
        s : Int
        s =
            if abs y > abs x then
                y

            else
                x
    in
    if s >= 0 then
        4 * s ^ 2 - x + y

    else
        let
            d : number
            d =
                if s == x then
                    1

                else
                    0
        in
        (4 * s ^ 2) + (-1 ^ d) * (2 * s + x + y)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Size size ->
            { model | size = size, history = compute model.size model.pieces }


compute : Int -> List Piece -> List Board
compute size pieces =
    let
        halfSize =
            size // 2

        initial : Board
        initial =
            { cells = Array.repeat (size * size) Open
            , openList =
                allCells size
                    (\x y ->
                        ( toSpiral x y, ( x, y, Nothing ) )
                    )
                    |> Dict.fromList
            }
    in
    if List.isEmpty pieces then
        [ initial ]

    else
        computeHelp pieces pieces [ initial ] initial


allCells : Int -> (Int -> Int -> a) -> List a
allCells size f =
    let
        halfSize =
            size // 2
    in
    List.range -halfSize halfSize
        |> List.concatMap
            (\y ->
                List.range -halfSize halfSize
                    |> List.map
                        (\x -> f x y)
            )


computeHelp : List Piece -> List Piece -> List Board -> Board -> List Board
computeHelp queue pieces acc board =
    case queue of
        [] ->
            computeHelp pieces pieces acc board

        headPiece :: tailPieces ->
            case step headPiece board of
                Nothing ->
                    List.reverse acc

                Just newBoard ->
                    computeHelp tailPieces pieces (newBoard :: acc) newBoard


step : Piece -> Board -> Maybe Board
step headPiece board =
    case
        Dict.stoppableFoldl
            (\s ( x, y, existing ) _ ->
                case existing of
                    Just color ->
                        if color == headPiece.color then
                            Dict.Stop (Just ( s, x, y ))

                        else
                            Dict.Continue Nothing

                    Nothing ->
                        Dict.Stop (Just ( s, x, y ))
            )
            Nothing
            board.openList
    of
        Nothing ->
            Nothing

        Just ( s, x, y ) ->
            let
                newOpenList =
                    List.foldl
                        (\( dx, dy ) acc ->
                            let
                                ds =
                                    toSpiral (x + dx) (y + dy)
                            in
                            case Dict.get ds acc of
                                Nothing ->
                                    acc

                                Just ( ex, ey, Nothing ) ->
                                    Dict.insert ds ( ex, ey, Just headPiece.color ) acc

                                Just ( ex, ey, Just ec ) ->
                                    if ec == headPiece.color then
                                        acc

                                    else
                                        Dict.remove ds acc
                        )
                        (Dict.remove s board.openList)
                        headPiece.moves
            in
            { cells = Array.set s (Colored headPiece.color) board.cells
            , openList = newOpenList
            }
                |> Just
