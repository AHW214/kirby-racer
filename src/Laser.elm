module Laser exposing
    ( Laser
    , tickAll
    , fire
    , viewAll
    )

import Object exposing (Object)
import Vector exposing (Vector, add)
import Html exposing (Html)
import Svg exposing (g)

type Damage
    = Weak
    | Medium
    | Strong

type alias Laser =
    Object { damage : Damage }

tickAll : Float -> List Laser -> List Laser
tickAll dt =
    List.filterMap (tick dt)

tick : Float -> Laser -> Maybe Laser
tick dt laser =
    let
        lsr =
            Object.tick dt laser
    in
        if Object.onScreen lsr then
            Just lsr
        else
            Nothing

fire : Float -> Vector -> Vector -> Laser
fire chargeTime pos vel =
    let
        (damage, rad) =
            if chargeTime < 0.5 then
                (Weak, 10)
            else if chargeTime < 1.0 then
                (Medium, 20)
            else
                (Strong, 30)
    in
        { pos = pos
        , vel = Vector.add vel (5000, 0)
        , acc = (0,0)
        , rad = rad
        , damage = damage
        }

laserImage : Laser -> String
laserImage { damage } =
    case damage of
        Weak   -> "weak"
        Medium -> "medium"
        Strong -> "strong"

viewAll : List Laser -> Html msg
viewAll lasers =
    g [] (List.map view lasers)


view : Laser -> Html msg
view laser =
    let
        img =
            "./assets/images/kirby/laser/" ++ laserImage laser ++ "-beam.png"
    in
        Object.view laser img (50, 50)