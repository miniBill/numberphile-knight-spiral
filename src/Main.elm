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
            -- [ knight (Color.rgb 1 0.6 0.6)
            -- , knight (Color.rgb 0.5 0.5 0.5)
            -- ]
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
    (viewOpenList board ++ viewBoardCells board)
        |> S.svg
            [ Html.Attributes.style "width" "50%"
            , Html.Attributes.style "margin" "auto"
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "font-size" "0.2px"
            , TypedSvg.Attributes.InPx.strokeWidth 0.01
            , SA.viewBox
                -(toFloat board.size + 0.5)
                -(toFloat board.size + 0.5)
                (toFloat (board.size * 2 + 1))
                (toFloat (board.size * 2 + 1))
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

                        Just (Controlled c) ->
                            c

                        Just Unusable ->
                            Color.gray
            in
            viewCell s x y color


viewCell : Int -> Int -> Int -> Color -> Svg msg
viewCell s x y color =
    let
        rect =
            S.rect
                [ TypedSvg.Attributes.InPx.x (toFloat x - 0.5)
                , TypedSvg.Attributes.InPx.y (toFloat y - 0.5)
                , TypedSvg.Attributes.InPx.width 1
                , TypedSvg.Attributes.InPx.height 1
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
            d : Int
            d =
                if s - x == 0 then
                    1

                else
                    0
        in
        (4 * s ^ 2) + (-1 ^ d) * (2 * s + x + y)


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
            , openList =
                allCells size
                    (\x y ->
                        ( toSpiral x y, ( x, y, Nothing ) )
                    )
                    |> Dict.fromList
            }
    in
    if List.isEmpty pieces then
        initial

    else
        computeHelp pieces pieces initial


allCells : Int -> (Int -> Int -> a) -> List a
allCells size f =
    let
        go : Int -> q -> (Int -> q -> q) -> q
        go v acc inner =
            if v > size then
                acc

            else
                go (v + 1) (inner v acc) inner
    in
    go -size [] (\y yacc -> go -size yacc (\x xacc -> f x y :: xacc))


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
    case findOpenCell headPiece board of
        Nothing ->
            Nothing

        Just ( s, x, y ) ->
            { size = board.size
            , cells = Array.set s (Colored headPiece.color) board.cells
            , openList = updateOpenList headPiece s x y board
            }
                |> Just


updateOpenList : Piece -> Int -> Int -> Int -> Board -> Dict Int ( Int, Int, Maybe Color )
updateOpenList headPiece s x y board =
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


findOpenCell : Piece -> Board -> Maybe ( Int, Int, Int )
findOpenCell headPiece board =
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
