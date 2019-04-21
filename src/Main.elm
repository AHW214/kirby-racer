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
import Vector exposing (Vector)

-- Params

type alias Params =
    { acc : Float
    , drag : Float
    , maxSpeed : Float
    , window : { height : Int, width : Int }
    }

params : Params
params =
    { acc = 10000
    , drag = 5000
    , maxSpeed = 2000
    , window = { height = 1920, width = 1080 }
    }


-- Model

type alias Model =
    { kirby : Kirby
    , obstacles : List Obstacle
    , input : Input
    }

type alias Input =
    { up : Float
    , down : Float
    , left : Float
    , right : Float
    }

type alias Kirby =
    Object {}

type alias Obstacle =
    Object {}

type alias Object a =
    { a
        | pos : Vector
        , vel : Vector
    }

object : Vector -> Vector -> Object {}
object pos vel =
    { pos = pos, vel = vel }

initModel : Model
initModel =
    { kirby = object (0, 0) (0, 0)
    , obstacles = []
    , input = { up = 0, down = 0, left = 0, right = 0 }
    }

init : Flags -> (Model, Cmd Msg)
init () = (initModel, Cmd.none)


-- Update

type Msg
    = Frame Float
    | Change Float String

setInput : Float -> String -> Input -> Input
setInput status key input =
    case key of
        "w" -> { input | up = status }
        "a" -> { input | left = status }
        "s" -> { input | down = status }
        "d" -> { input | right = status }
        _   -> input

sumInput : Input -> Vector
sumInput { up, down, left, right } =
    Vector.norm (right - left, up - down)

drag : Float -> Float -> Float -> Object a -> Object a
drag dt d mx obj =
    let
        { vel } = obj
        speed = Vector.mag vel
        newSpeed = clamp 0 mx (speed - dt * d)
        k = if speed > 0 then newSpeed / speed else 0
    in
        { obj | vel = Vector.scale k vel }

velocity : Float -> Vector -> Object a -> Object a
velocity dt acc obj =
    { obj | vel = Vector.add obj.vel <| Vector.scale dt acc }

position : Float -> Object a -> Object a
position dt obj =
    { obj | pos = Vector.add obj.pos <| Vector.scale dt obj.vel }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Frame ms ->
            let
                dt = ms / 1000
                acc = Vector.scale params.acc (sumInput model.input)
                kirby =
                    model.kirby
                        |> velocity dt acc
                        |> drag dt params.drag params.maxSpeed
                        |> position dt
            in
                ( { model | kirby = kirby }
                , Cmd.none
                )
        Change status key ->
            ( { model | input = setInput status key model.input }
            , Cmd.none
            )


-- View

display : Model -> Collage Msg
display { kirby } =
    let
        circ =
            circle 5
                |> filled (uniform Color.white)
                |> shift kirby.pos
        rect =
            rectangle 1920 1080
                |> filled (uniform Color.black)
    in
        impose circ rect

view : Model -> Browser.Document Msg
view model =
    { title = "Kirby Racer"
    , body = [ svg (display model), Html.h1 [] [ Html.text <| Debug.toString (Vector.mag model.kirby.vel) ] ]
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