module Kirby exposing
    ( Kirby
    , init
    , speed
    , tick
    , view
    )

import Object exposing (Object)
import Html exposing (Html)

type alias Kirby =
    Object
        { hp : Int
        , chargeTime : Float
        , firing : Bool
        , collision : Bool
        }

init : Kirby
init =
    { pos = (0, 0)
    , vel = (0, 0)
    , acc = (0, 0)
    , rad = 24 -- dont hardcode
    , hp = 10  -- dont hardcode
    , chargeTime = 0
    , firing = False
    , collision = False
    }

speed : Float
speed = 10000

tick : Float -> Kirby -> Kirby
tick dt =
    Object.drag dt 5000 2000 << Object.tick dt << laserState

laserState : Kirby -> Kirby
laserState ({ firing, chargeTime } as kirby) =
    if firing then
        { kirby | firing = False, chargeTime = 0 }
    else
        kirby

moveImage : Kirby -> String
moveImage { acc } =
    let
        (ax, ay) = acc

        name =
            if ay > 0 then
                "up"
            else if ay < 0 then
                "down"
            else if ax > 0 then
                "right"
         {- else if ax < 0 then
                "left" -}
            else
                "still"
    in
        "move/" ++ name

laserImage : Kirby -> String
laserImage { firing, chargeTime } =
    let
        name =
            if chargeTime < 0.5 then
                "weak"
            else if chargeTime < 1.0 then
                "medium"
            else
                "strong"
        action =
            if firing then
                "fire"
            else
                "charge"
    in
        "laser/" ++ action ++ "-" ++ name

view : Kirby -> Html msg
view ({ chargeTime } as kirby) =
    let
        path =
            if chargeTime == 0 then
                moveImage kirby
            else
                laserImage kirby

        img =
            "./assets/images/kirby/" ++ path ++ ".gif"
    in
        Object.view kirby img (50, 50)