module Color exposing (Color, black, blue, gray, purple, red, toCssString, white)

import Bitwise


type alias Color =
    Int


toCssString : Color -> String
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


black : Color
black =
    0


red : Color
red =
    0x00FF0000


blue : Color
blue =
    0xFF


purple : Color
purple =
    0x00FF00FF


gray : Color
gray =
    0x00D0D0D0


white : Color
white =
    0x00FFFFFF
