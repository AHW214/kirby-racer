module Object exposing
    ( Object
    , tick
    , drag
    , onScreen
    , isColliding
    , view
    )

import Utils
import Vector exposing (Vector)
import Html exposing (Html)
import Svg exposing (image)
import Svg.Attributes exposing (x, y, width, height, xlinkHref)

type alias Object a =
    { a
        | pos : Vector
        , vel : Vector
        , acc : Vector
        , rad : Float
    }

tick : Float -> Object a -> Object a
tick dt =
    updatePosition dt << updateVelocity dt

drag : Float -> Float -> Float -> Object a -> Object a
drag dt d mx obj =
    let
        { vel } = obj
        speed = Vector.mag vel
        newSpeed = clamp 0 mx (speed - dt * d)
        k = if speed > 0 then newSpeed / speed else 0
    in
        { obj | vel = Vector.scale k vel }

updatePosition : Float -> Object a -> Object a
updatePosition dt obj =
    { obj | pos = Vector.add obj.pos <| Vector.scale dt obj.vel }

updateVelocity : Float -> Object a -> Object a
updateVelocity dt obj =
    { obj | vel = Vector.add obj.vel <| Vector.scale dt obj.acc }

setAcceleration : Vector -> Object a -> Object a
setAcceleration acc obj =
    { obj | acc = acc }

onScreen : Object a -> Bool
onScreen { pos, rad } =
    let
        (x, y) = pos
        { w, h } = Utils.window
        (threshX, threshY) = (w + rad, h + rad)
    in
        x <= threshX && x >= 0 && y <= threshY && y >= 0

isColliding : Object a -> Object b -> Bool
isColliding obj1 obj2 =
    obj1.rad + obj2.rad > Vector.dist obj1.pos obj2.pos

view : Object a -> String -> (Float, Float) -> Html msg
view { pos } img ( w, h ) =
    let
        (px, py) = pos

        rx =
            String.fromFloat (px - (w / 2))

        ry =
            String.fromFloat (1080 - (py - (h / 2)))
    in
        image
            [ x rx
            , y ry
            , width (String.fromFloat w)
            , height (String.fromFloat h)
            , xlinkHref img
            ]
            []


