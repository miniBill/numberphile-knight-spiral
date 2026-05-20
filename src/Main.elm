module Main exposing (compute, defaultPieces, main)

import Array exposing (Array)
import Browser
import Canvas
import Canvas.Settings
import Canvas.Settings.Advanced
import Color
import Common exposing (allCells, toSpiral)
import FastDict as Dict exposing (Dict)
import FastSet as Set exposing (Set)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import IColor exposing (IColor)
import OpenList exposing (OpenList)


type alias Model =
    { pieces : List Piece
    , board : Board
    }


type alias Piece =
    { name : String
    , color : IColor
    , moves : List ( Int, Int )
    }


type alias Board =
    { size : Int
    , cells : Array Cell
    , openList : OpenList
    }


type Cell
    = Open
    | Colored IColor


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
    [ knight IColor.black
    , knight IColor.red
    ]


init : Model
init =
    let
        pieces : List Piece
        pieces =
            [--     wazir Color.black
             -- , ferz Color.red
             -- , wazir Color.blue
             -- , ferz Color.purple
             -- , knight Color.black
             -- , zebra Color.red
             -- , dabbaba Color.red
             -- , wazir Color.blue
             -- , wazir Color.purple
            ]
                ++ defaultPieces
    in
    { pieces = pieces
    , board = compute 30 pieces
    }


knight : IColor -> Piece
knight color =
    { name = "Knight"
    , color = color
    , moves = [ ( 2, 1 ), ( 1, 2 ), ( 2, -1 ), ( -1, 2 ), ( -2, 1 ), ( 1, -2 ), ( -2, -1 ), ( -1, -2 ) ]
    }


zebra : IColor -> Piece
zebra color =
    { name = "Zebra"
    , color = color
    , moves = [ ( 2, 3 ), ( 3, 2 ), ( 2, -3 ), ( -3, 2 ), ( -2, 3 ), ( 3, -2 ), ( -2, -3 ), ( -3, -2 ) ]
    }


dabbaba : IColor -> Piece
dabbaba color =
    { name = "Dabbaba"
    , color = color
    , moves = [ ( 0, 2 ), ( 0, -2 ), ( 2, 0 ), ( -2, 0 ) ]
    }


wazir : IColor -> Piece
wazir color =
    { name = "Wazir"
    , color = color
    , moves = [ ( 0, 1 ), ( 0, -1 ), ( 1, 0 ), ( -1, 0 ) ]
    }


ferz : IColor -> Piece
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
                            , Html.Attributes.style "background" (IColor.toCssString piece.color)
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
    let
        scale : Float
        scale =
            800
    in
    viewBoardCells scale board
        |> (::) (Canvas.shapes [ Canvas.Settings.fill Color.white ] [ Canvas.rect ( 0, 0 ) scale scale ])
        |> Canvas.toHtml ( ceiling scale, ceiling scale )
            [ Html.Attributes.style "width" (String.fromInt (ceiling scale) ++ "px")
            , Html.Attributes.style "height" (String.fromInt (ceiling scale) ++ "px")
            , Html.Attributes.style "display" "block"
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "font-size" "0.2px"
            , Html.Attributes.style "transform" "rotate(90deg) scale(-1, 1)"
            ]


viewBoardCells : Float -> Board -> List Canvas.Renderable
viewBoardCells scale board =
    allCells board.size <|
        \x y ->
            let
                s : Int
                s =
                    toSpiral x y

                color : IColor
                color =
                    case Array.get s board.cells of
                        Nothing ->
                            IColor.red

                        Just Open ->
                            IColor.white

                        Just (Colored c) ->
                            c
            in
            viewCell scale board.size s x y color


viewCell : Float -> Int -> Int -> Int -> Int -> IColor -> Canvas.Renderable
viewCell scale boardSize s x y color =
    let
        cellSize =
            scale / (toFloat boardSize * 2 + 1)
    in
    Canvas.shapes
        [ Canvas.Settings.fill (IColor.toColor color) ]
        [ Canvas.rect
            ( toFloat (x + boardSize) * cellSize
            , toFloat (y + boardSize) * cellSize
            )
            cellSize
            cellSize
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
        (board.openList
            |> OpenList.remove s
            |> OpenList.setMinForColor headPiece.color (s + 1)
        )
        headPiece.moves
