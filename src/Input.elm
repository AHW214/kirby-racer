module Input exposing
    ( Input
    , init
    , set
    , toDir
    )

import Vector exposing (Vector)

type alias Input =
    { up : Bool
    , down : Bool
    , left : Bool
    , right : Bool
    , space : Bool
    }

init : Input
init =
    { up = False
    , down = False
    , left = False
    , right = False
    , space = False
    }

set : Bool -> String -> Input -> Input
set status key input =
    case key of
        "w" -> { input | up = status  }
        "a" -> { input | left = status }
        "s" -> { input | down = status }
        "d" -> { input | right = status }
        " " -> { input | space = status }
        _   -> input

isCharging : Input -> Bool
isCharging { space } = space

toDir : Input -> Vector
toDir { up, down, left, right } =
    let
        vertical = toFloat up - toFloat down
        horizontal = toFloat right - toFloat left
    in
        Vector.norm (horizontal, vertical)

toFloat : Bool -> Float
toFloat b = if b then 1 else 0