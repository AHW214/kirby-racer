module Obstacle exposing
    ( Obstacle
    , tickAll
    , generator
    , viewAll
    )

import Object exposing (Object)
import Vector exposing (Vector, random)
import Utils
import Html exposing (Html)
import Svg exposing (g, circle) -- remove
import Svg.Attributes exposing (cx, cy, r, fill) -- remove
import Random

type alias Obstacle =
    Object {}

tickAll : Float -> List Obstacle -> List Obstacle
tickAll dt =
    List.filterMap (tick dt)

tick : Float -> Obstacle -> Maybe Obstacle
tick dt obstacle =
    let
        obs =
            Object.tick dt obstacle
    in
        if Object.onScreen obs then
            Just obs
        else
            Nothing

maxRadius : Float
maxRadius = 30

minRadius : Float
minRadius = 5

generator : Random.Generator Obstacle
generator =
    let spawnX = Utils.window.w + maxRadius in
    Random.map3
        (\p v r -> { pos = p, vel = v, rad = r, acc = (0,0) })
        (Vector.random (spawnX, 0) (spawnX, Utils.window.h))
        (Vector.random (-1000, -50) (-200, 50))
        (Random.float minRadius maxRadius)

viewAll : List Obstacle -> Html msg
viewAll obstacles =
    g [] (List.map view obstacles)

view : Obstacle -> Html msg -- use Object.view later
view { pos, rad } =
    let
        (px, py) = pos
    in
        circle
            [ cx (String.fromFloat px)
            , cy (String.fromFloat py)
            , r (String.fromFloat rad)
            , fill "red"
            ]
            []