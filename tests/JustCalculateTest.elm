module JustCalculateTest exposing (suite)

import Expect
import Main
import Test exposing (Test)


suite : Test
suite =
    Test.test "Just run Main.compute" <|
        \_ ->
            let
                _ =
                    Main.compute 200 Main.defaultPieces
            in
            Expect.pass
