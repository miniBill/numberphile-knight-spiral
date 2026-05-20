module Common exposing (allCells, toSpiral)


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
