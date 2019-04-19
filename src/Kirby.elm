module Main exposing (..)

-- Imports

import Browser
import Browser.Events
import Collage exposing (..)
import Collage.Layout exposing (impose)
import Collage.Render exposing (svg)
import Color exposing (Color)
import Html exposing (Html)
import Task
import Time exposing (..)
import Json.Decode as Decode
import Set exposing (Set)

-- Params

type alias Params =
    { acc: Float    -- px/sec^2
    , drag : Float
    }

params : Params
params =
    { acc = 10000
    , drag = 50
    }

-- Model

type alias Model =
    { pos : Vector2
    , vel : Vector2
    , acc : Vector2
    , dirs : Directions
    }

type alias Vector2 =
    { x : Float, y: Float }

type alias Directions =
    { up : Float
    , down : Float
    , left : Float
    , right : Float
    }

initKirby : Model
initKirby =
    { pos = Vector2 0 0
    , vel = Vector2 0 0
    , acc = Vector2 0 0
    , dirs = Directions 0 0 0 0
    }

init : Flags -> (Model, Cmd Msg)
init () = (initKirby, Cmd.none)


-- Update

type Msg
    = Frame Float
    | Change Float String

directions : Float -> String -> Directions -> Directions
directions status key dirs =
    case key of
        "w" -> { dirs | up = status }
        "a" -> { dirs | left = status }
        "s" -> { dirs | down = status }
        "d" -> { dirs | right = status }
        _   -> dirs

magnitude : Vector2 -> Float
magnitude { x , y } = sqrt (x^2 + y^2)

scalar : Float -> Vector2 -> Vector2
scalar k vector =
    { vector
        | x = k * vector.x
        , y = k * vector.y
    }

normalize : Vector2 -> Vector2
normalize vector =
    let m = magnitude vector in
    if m > 0 then scalar (1 / m) vector else vector

input : Directions -> Vector2
input { up, down, left, right } =
    { x = right - left
    , y = up - down
    }

drag : Float -> Model -> Model
drag d kirby =
    let
        { vel } = kirby
        speed = magnitude vel
        newSpeed = max 0 (speed - d)
        scale = if speed > 0 then newSpeed / speed else 0
    in
        { kirby | vel = scalar scale vel}

acceleration : Model -> Model
acceleration kirby =
    let
        { acc, dirs } = kirby
        norm = normalize (input dirs)
        newAcc =
            { acc
                | x = params.acc * norm.x
                , y = params.acc * norm.y
            }
    in
        { kirby | acc = newAcc }

velocity : Float -> Model -> Model
velocity dt kirby =
    let
        { acc, vel } = kirby
        newVel =
            { vel
                | x = vel.x + dt * acc.x
                , y = vel.y + dt * acc.y
            }
    in
        { kirby | vel = newVel }

position : Float -> Model -> Model
position dt kirby =
    let
        { vel , pos } = kirby
        newPos =
            { pos
                | x = pos.x + dt * vel.x
                , y = pos.y + dt * vel.y
            }
    in
        { kirby | pos = newPos }

update : Msg -> Model -> (Model, Cmd Msg)
update msg kirby =
    case msg of
        Frame ms ->
            let dt = ms / 1000 in
            ( kirby
                |> acceleration
                |> velocity dt
                |> drag params.drag
                |> position dt
            , Cmd.none
            )
        Change status key ->
            ( { kirby | dirs = directions status key kirby.dirs}
            , Cmd.none
            )


-- View

display : Model -> Collage Msg
display { pos } =
    let
        circ =
            circle 5
                |> filled (uniform Color.white)
                |> shift (pos.x, pos.y)
        rect =
            rectangle 1920 1080
                |> filled (uniform Color.black)
    in
        impose circ rect

view : Model -> Browser.Document Msg
view kirby =
    { title = "Kirby Racer"
    , body = [ svg (display kirby), Html.h1 [] [Html.text <| Debug.toString kirby.pos.x] ]
    }


-- Subscriptions

keyDecoder : (String -> Msg) -> Decode.Decoder Msg
keyDecoder action =
    Decode.map action (Decode.field "key" Decode.string)

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyUp <| keyDecoder <| Change 0
        , Browser.Events.onKeyDown <| keyDecoder <| Change 1
        , Browser.Events.onAnimationFrameDelta Frame
        ]


-- Program

type alias Flags = ()

main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }