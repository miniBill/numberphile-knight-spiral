module OpenList exposing (OpenList, findOpenCell, get, init, insert, remove, toList)

import Color exposing (Color)
import Common
import FastDict as Dict exposing (Dict)


type OpenList
    = OpenList (Dict Int { x : Int, y : Int, color : Maybe Color })


toList : OpenList -> List ( Int, { x : Int, y : Int, color : Maybe Color } )
toList (OpenList dict) =
    Dict.toList dict


init : Int -> OpenList
init size =
    Common.allCells size
        (\x y ->
            ( Common.toSpiral x y, { x = x, y = y, color = Nothing } )
        )
        |> Dict.fromList
        |> OpenList


remove : Int -> OpenList -> OpenList
remove s (OpenList dict) =
    OpenList (Dict.remove s dict)


get : Int -> OpenList -> Maybe { x : Int, y : Int, color : Maybe Color }
get s (OpenList dict) =
    Dict.get s dict


insert : Int -> Int -> Int -> Maybe Color -> OpenList -> OpenList
insert s x y color (OpenList dict) =
    OpenList (Dict.insert s { x = x, y = y, color = color } dict)


findOpenCell : Color -> OpenList -> Maybe { s : Int, x : Int, y : Int }
findOpenCell forColor (OpenList dict) =
    Dict.stoppableFoldl
        (\s existing _ ->
            case existing.color of
                Just color ->
                    if color == forColor then
                        Dict.Stop (Just { s = s, x = existing.x, y = existing.y })

                    else
                        Dict.Continue Nothing

                Nothing ->
                    Dict.Stop (Just { s = s, x = existing.x, y = existing.y })
        )
        Nothing
        dict
