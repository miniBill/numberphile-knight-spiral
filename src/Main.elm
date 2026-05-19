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
import TypedSvg.Core as S
import TypedSvg.Types exposing (AnchorAlignment(..), DominantBaseline(..), Paint(..))


type alias Model =
    { pieces : List Piece
    , size : Int
    , board : Board
    }


type alias Piece =
    { name : String
    , color : Color
    , moves : List ( Int, Int )
    }


type alias Board =
    { cells : Array Cell
    , openList : Dict Int ( Int, Int )
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
            [ knight Color.red
            , knight Color.black
            ]
    in
    { pieces = pieces
    , size = 7
    , board = compute 7 pieces
    }


knight : Color -> Piece
knight color =
    { name = "Knight"
    , color = color
    , moves = [ ( 2, 1 ), ( 1, 2 ), ( 2, -1 ), ( -1, 2 ), ( -2, 1 ), ( 1, -2 ), ( -2, -1 ), ( -1, 2 ) ]
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
        , viewBoard model.board
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
    List.range -halfEdge halfEdge
        |> List.concatMap
            (\y ->
                List.range -halfEdge halfEdge
                    |> List.map
                        (\x ->
                            let
                                s =
                                    toSpiral x y

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
                            S.g []
                                [ S.rect
                                    [ TypedSvg.Attributes.InPx.x (toFloat x - 0.5)
                                    , TypedSvg.Attributes.InPx.y (toFloat y - 0.5)
                                    , TypedSvg.Attributes.InPx.width 1
                                    , TypedSvg.Attributes.InPx.height 1
                                    , SA.fill PaintNone
                                    , SA.stroke (Paint color)
                                    ]
                                    []
                                , S.text_
                                    [ TypedSvg.Attributes.InPx.x (toFloat x)
                                    , TypedSvg.Attributes.InPx.y (toFloat y)
                                    , SA.textAnchor AnchorMiddle
                                    , SA.dominantBaseline DominantBaselineMiddle
                                    ]
                                    [ S.text
                                        ("("
                                            ++ String.fromInt x
                                            ++ ", "
                                            ++ String.fromInt y
                                            ++ ") "
                                            ++ String.fromInt s
                                        )
                                    ]
                                ]
                        )
            )
        |> S.svg
            [ Html.Attributes.style "width" "50%"
            , Html.Attributes.style "margin" "auto"
            , Html.Attributes.style "border" "1px solid black"
            , Html.Attributes.style "font-size" "0.2px"
            , TypedSvg.Attributes.InPx.strokeWidth 0.01
            , SA.viewBox -(toFloat halfEdge + 0.5) -(toFloat halfEdge + 0.5) (toFloat edge) (toFloat edge)
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
            { model | size = size, board = compute model.size model.pieces }


compute : Int -> List Piece -> Board
compute size pieces =
    let
        halfSize =
            size // 2

        initial : Board
        initial =
            { cells = Array.repeat (size * size) Open
            , openList =
                List.range -halfSize halfSize
                    |> List.concatMap
                        (\y ->
                            List.range -halfSize halfSize
                                |> List.map
                                    (\x ->
                                        ( toSpiral x y, ( x, y ) )
                                    )
                        )
                    |> Dict.fromList
            }
    in
    if List.isEmpty pieces then
        initial

    else
        computeHelp 0 0 0 North pieces pieces initial


type Direction
    = North
    | West
    | South
    | East


nextDirection : Direction -> Direction
nextDirection direction =
    case direction of
        North ->
            East

        East ->
            South

        South ->
            West

        West ->
            North


computeHelp : Int -> Int -> Int -> Direction -> List Piece -> List Piece -> Board -> Board
computeHelp x y s direction queue pieces board =
    if s >= Array.length board.cells then
        board

    else
        case queue of
            [] ->
                computeHelp x y s direction pieces pieces board

            headPiece :: tailPieces ->
                let
                    ( dx, dy ) =
                        towards direction x y

                    ( nx, ny ) =
                        towards (nextDirection direction) x y

                    ( newX, newY, newDirection ) =
                        if toSpiral dx dy == s + 1 then
                            ( dx, dy, direction )

                        else if toSpiral nx ny == s + 1 then
                            ( nx, ny, nextDirection direction )

                        else
                            let
                                _ =
                                    Debug.log "x" x

                                _ =
                                    Debug.log "y" y

                                _ =
                                    Debug.log "s" s

                                _ =
                                    Debug.log "dx" dx

                                _ =
                                    Debug.log "dy" dy

                                _ =
                                    Debug.log "s(dx,dy)" (toSpiral dx dy)

                                _ =
                                    Debug.log "nx" nx

                                _ =
                                    Debug.log "ny" ny

                                _ =
                                    Debug.log "s(nx,ny)" (toSpiral nx ny)
                            in
                            ( x, y, direction )
                in
                computeHelp newX
                    newY
                    (s + 1)
                    newDirection
                    tailPieces
                    pieces
                    { cells = board.cells
                    , openList = board.openList
                    }


towards : Direction -> Int -> Int -> ( Int, Int )
towards direction x y =
    case direction of
        North ->
            ( x, y - 1 )

        West ->
            ( x - 1, y )

        South ->
            ( x, y + 1 )

        East ->
            ( x + 1, y )
