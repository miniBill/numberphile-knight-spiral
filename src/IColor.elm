module IColor exposing (IColor, black, blue, fromCssString, gray, purple, red, toColor, toCssString, white)

import Bitwise
import Color exposing (Color)
import Hex


type alias IColor =
    Int


toCssString : IColor -> String
toCssString c =
    let
        r =
            c |> Bitwise.shiftRightBy 16

        g =
            c |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF

        b =
            c |> Bitwise.and 0xFF
    in
    "rgb(" ++ String.fromInt r ++ " " ++ String.fromInt g ++ " " ++ String.fromInt b ++ ")"


black : IColor
black =
    0


red : IColor
red =
    0x00FF0000


blue : IColor
blue =
    0xFF


purple : IColor
purple =
    0x00FF00FF


gray : IColor
gray =
    0x00D0D0D0


white : IColor
white =
    0x00FFFFFF


toColor : IColor -> Color
toColor c =
    let
        r =
            c |> Bitwise.shiftRightBy 16

        g =
            c |> Bitwise.shiftRightBy 8 |> Bitwise.and 0xFF

        b =
            c |> Bitwise.and 0xFF
    in
    Color.rgb255 r g b


fromCssString : String -> Maybe IColor
fromCssString s =
    Result.map3 (\rr gg bb -> Bitwise.shiftLeftBy 16 rr + Bitwise.shiftLeftBy 8 gg + bb)
        (String.slice 1 3 s |> Hex.fromString)
        (String.slice 3 5 s |> Hex.fromString)
        (String.slice 5 7 s |> Hex.fromString)
        |> Result.toMaybe
