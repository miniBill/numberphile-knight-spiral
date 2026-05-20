module IColor exposing (IColor, black, blue, gray, purple, red, toColor, toCssString, white)

import Bitwise
import Color exposing (Color)


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
