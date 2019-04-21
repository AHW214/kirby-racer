module Vector exposing
    ( Vector
    , add
    , sub
    , mult
    , div
    , dot
    , scale
    , mag
    , norm
    , dist
    , random
    )

import Random exposing (Generator)

type alias Vector = (Float, Float)

biOp : (Float -> Float -> Float) -> Vector -> Vector -> Vector
biOp op (x1, y1) (x2, y2) =
    (op x1 x2, op y1 y2)

add : Vector -> Vector -> Vector
add = biOp (+)

sub : Vector -> Vector -> Vector
sub = biOp (-)

mult : Vector -> Vector -> Vector
mult = biOp (*)

div : Vector -> Vector -> Vector
div = biOp (/)

dot : Vector -> Vector -> Float
dot (x1, y1) (x2, y2) =
    (x1 * x2) + (y1 * y2)

scale : Float -> Vector -> Vector
scale k (x, y) = (k * x, k * y)

mag : Vector -> Float
mag (x, y) = sqrt (x^2 + y^2)

norm : Vector -> Vector
norm (x, y) =
    let m = mag (x, y) in
    if m > 0 then scale (1 / m) (x, y) else (0, 0)

dist : Vector -> Vector -> Float
dist v1 v2 = mag <| sub v1 v2

random : Vector -> Vector -> Generator Vector
random (minX, minY) (maxX, maxY) =
    Random.pair
        (Random.float minX maxX)
        (Random.float minY maxY)