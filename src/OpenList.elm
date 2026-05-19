module OpenList exposing (OpenList, findOpenCell, get, init, insert, remove, setMinForColor, toList)

import Array exposing (Array)
import Color exposing (Color)
import Common
import FastDict as Dict exposing (Dict)


type OpenList
    = OpenList (Dict Int Int) (Dict Int { x : Int, y : Int, color : Maybe Color })


toList : OpenList -> List ( Int, { x : Int, y : Int, color : Maybe Color } )
toList (OpenList _ dict) =
    Dict.toList dict


init : Int -> OpenList
init size =
    Common.allCells size
        (\x y ->
            ( Common.toSpiral x y, { x = x, y = y, color = Nothing } )
        )
        |> Dict.fromList
        |> OpenList Dict.empty


remove : Int -> OpenList -> OpenList
remove s (OpenList l dict) =
    OpenList l (Dict.remove s dict)


get : Int -> OpenList -> Maybe { x : Int, y : Int, color : Maybe Color }
get s (OpenList _ dict) =
    Dict.get s dict


insert : Int -> Int -> Int -> Maybe Color -> OpenList -> OpenList
insert s x y color (OpenList l dict) =
    OpenList l (Dict.insert s { x = x, y = y, color = color } dict)


findOpenCell : Color -> OpenList -> Maybe { s : Int, x : Int, y : Int }
findOpenCell forColor (OpenList l dict) =
    let
        from : Int
        from =
            Dict.get forColor l
                |> Maybe.withDefault 0
    in
    Dict.restructure Nothing
        (\{ key, value, left, right } ->
            let
                here () =
                    case value.color of
                        Just color ->
                            if color == forColor then
                                Just { s = key, x = value.x, y = value.y }

                            else
                                Nothing

                        Nothing ->
                            Just { s = key, x = value.x, y = value.y }
            in
            if key - from < 0 then
                -- key < from
                right ()

            else if key > from then
                case left () of
                    Just v ->
                        Just v

                    Nothing ->
                        case here () of
                            Just v ->
                                Just v

                            Nothing ->
                                right ()

            else
                case here () of
                    Just v ->
                        Just v

                    Nothing ->
                        right ()
        )
        dict


setMinForColor : Color -> Int -> OpenList -> OpenList
setMinForColor c mn (OpenList mdict dict) =
    OpenList (Dict.insert c mn mdict) dict
