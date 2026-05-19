module Main exposing (compute, defaultPieces, main)

import Array exposing (Array)
import Browser
import Color exposing (Color)
import Common exposing (allCells, toSpiral)
import FastDict as Dict exposing (Dict)
import FastSet as Set exposing (Set)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import OpenList exposing (OpenList)
import Svg as S exposing (Svg)
import Svg.Attributes as SA


type alias Model =
    { pieces : List Piece
    , board : Board
    }


type alias Piece =
    { name : String
    , color : Color
    , moves : List ( Int, Int )
    }


type alias Board =
    { size : Int
    , cells : Array Cell
    , openList : OpenList
    }


type Cell
    = Open
    | Colored Color


type Msg
    = Size Int


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }


defaultPieces : List Piece
defaultPieces =
    [ knight Color.black
    , knight Color.red
    ]


init : Model
init =
    let
        pieces : List Piece
        pieces =
            [ wazir Color.black
            , ferz Color.red
            , wazir Color.blue
            , ferz Color.purple

            -- , knight Color.black
            -- , zebra Color.red
            -- , dabbaba Color.red
            -- , wazir Color.blue
            -- , wazir Color.purple
            ]
    in
    { pieces = pieces
    , board = compute 30 pieces
    }


knight : Color -> Piece
knight color =
    { name = "Knight"
    , color = color
    , moves = [ ( 2, 1 ), ( 1, 2 ), ( 2, -1 ), ( -1, 2 ), ( -2, 1 ), ( 1, -2 ), ( -2, -1 ), ( -1, -2 ) ]
    }


zebra : Color -> Piece
zebra color =
    { name = "Zebra"
    , color = color
    , moves = [ ( 2, 3 ), ( 3, 2 ), ( 2, -3 ), ( -3, 2 ), ( -2, 3 ), ( 3, -2 ), ( -2, -3 ), ( -3, -2 ) ]
    }


dabbaba : Color -> Piece
dabbaba color =
    { name = "Dabbaba"
    , color = color
    , moves = [ ( 0, 2 ), ( 0, -2 ), ( 2, 0 ), ( -2, 0 ) ]
    }


wazir : Color -> Piece
wazir color =
    { name = "Wazir"
    , color = color
    , moves = [ ( 0, 1 ), ( 0, -1 ), ( 1, 0 ), ( -1, 0 ) ]
    }


ferz : Color -> Piece
ferz color =
    { name = "Ferz"
    , color = color
    , moves = [ ( 1, 1 ), ( 1, -1 ), ( -1, 1 ), ( -1, -1 ) ]
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
                , Html.Attributes.value (String.fromInt model.board.size)
                , Html.Events.onInput (\v -> v |> String.toInt |> Maybe.withDefault model.board.size |> Size)
                ]
                []
            ]
        , viewBoard model.board
        ]


viewBoard : Board -> Html Msg
viewBoard board =
    (-- viewOpenList board ++
     viewBoardCells board
    )
        |> S.svg
            [ Html.Attributes.style "width" "50%"
            , Html.Attributes.style "margin" "auto"
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "font-size" "0.2px"
            , SA.strokeWidth "0.01px"
            , [ -(toFloat board.size + 0.5)
              , -(toFloat board.size + 0.5)
              , toFloat (board.size * 2 + 1)
              , toFloat (board.size * 2 + 1)
              ]
                |> List.map String.fromFloat
                |> String.join " "
                |> SA.viewBox
            ]


viewOpenList : Board -> List (Svg Msg)
viewOpenList board =
    board.openList
        |> OpenList.toList
        |> List.map
            (\( _, { x, y, color } ) ->
                S.circle
                    [ SA.cx (String.fromFloat (toFloat x) ++ "px")
                    , SA.cy (String.fromFloat (toFloat y) ++ "px")
                    , SA.r "0.1px"
                    , SA.fill (color |> Maybe.withDefault Color.gray |> Color.toCssString)
                    ]
                    []
            )


viewBoardCells : Board -> List (Svg msg)
viewBoardCells board =
    allCells board.size <|
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
            in
            viewCell s x y color


viewCell : Int -> Int -> Int -> Color -> Svg msg
viewCell s x y color =
    let
        rect =
            S.rect
                [ SA.x (String.fromFloat (toFloat x - 0.5) ++ "px")
                , SA.y (String.fromFloat (toFloat y - 0.5) ++ "px")
                , SA.width "1px"
                , SA.height "1px"
                , SA.fill (Paint color)
                ]
                []
    in
    if True then
        rect

    else
        S.g []
            [ rect
            , S.text_
                [ SA.x (String.fromFloat (toFloat x) ++ "px")
                , SA.y (String.fromFloat (toFloat y) ++ "px")
                , SA.textAnchor "middle"
                , SA.dominantBaseline "middle"
                ]
                [ "({x}, {y}) {s}"
                    |> String.replace "{x}" (String.fromInt x)
                    |> String.replace "{y}" (String.fromInt y)
                    |> String.replace "{s}" (String.fromInt s)
                    |> S.text
                ]
            ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        Size size ->
            { model | board = compute size model.pieces }


compute : Int -> List Piece -> Board
compute size pieces =
    let
        initial : Board
        initial =
            { size = size
            , cells = Array.repeat ((size * 2 + 1) ^ 2) Open
            , openList = OpenList.init size
            }
    in
    if List.isEmpty pieces then
        initial

    else
        computeHelp pieces pieces initial


computeHelp : List Piece -> List Piece -> Board -> Board
computeHelp queue pieces board =
    case queue of
        [] ->
            computeHelp pieces pieces board

        headPiece :: tailPieces ->
            case step headPiece board of
                Nothing ->
                    board

                Just newBoard ->
                    computeHelp tailPieces pieces newBoard


step : Piece -> Board -> Maybe Board
step headPiece board =
    case OpenList.findOpenCell headPiece.color board.openList of
        Nothing ->
            Nothing

        Just { s, x, y } ->
            { size = board.size
            , cells = Array.set s (Colored headPiece.color) board.cells
            , openList = updateOpenList headPiece s x y board
            }
                |> Just


updateOpenList : Piece -> Int -> Int -> Int -> Board -> OpenList
updateOpenList headPiece s x y board =
    List.foldl
        (\( dx, dy ) acc ->
            let
                ds =
                    toSpiral (x + dx) (y + dy)
            in
            case OpenList.get ds acc of
                Nothing ->
                    acc

                Just found ->
                    case found.color of
                        Nothing ->
                            OpenList.insert ds found.x found.y (Just headPiece.color) acc

                        Just ec ->
                            if ec == headPiece.color then
                                acc

                            else
                                OpenList.remove ds acc
        )
        (OpenList.remove s board.openList)
        headPiece.moves
