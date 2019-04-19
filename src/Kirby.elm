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


-- Model

type alias Model =
    { pos : Vector2
    , vel : Vector2
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

velocity : Directions -> Vector2 -> Vector2
velocity dirs vel =
    { vel
        | x = dirs.right - dirs.left
        , y = dirs.up - dirs.down
    }

position : Float -> Vector2 -> Vector2 -> Vector2
position dt vel pos =
    { pos
        | x = pos.x + dt * vel.x
        , y = pos.y + dt * vel.y
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg kirby =
    case msg of
        Frame dt ->
            ( { kirby
                | vel = velocity kirby.dirs kirby.vel
                , pos = position dt kirby.vel kirby.pos
              }
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
            rectangle 1000 1000
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
