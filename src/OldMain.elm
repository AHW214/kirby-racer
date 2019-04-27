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
import Random

-- Params

type alias Params =
    { acc : Float
    , drag : Float
    , maxSpeed : Float
    , obstacleSpawnTime : Float
    , maxObstacleRadius : Float
    , window : { height : Int, width : Int }
    }

params : Params
params =
    { acc = 10000
    , drag = 5000
    , maxSpeed = 2000
    , obstacleSpawnTime = 500
    , maxObstacleRadius = 30
    , window = { width = 1920, height = 1080 }
    }


-- Model

type alias Model =
    { kirby : Kirby
    , obstacles : List Obstacle
    , input : Input
    , health : Int
    }

type alias Input =
    { up : Float
    , down : Float
    , left : Float
    , right : Float
    }

type alias Kirby =
    Object { collision : Bool }

type alias Obstacle =
    Object {}

type alias Object a =
    { a
        | pos : Vector
        , vel : Vector
        , rad : Float
    }

object : Vector -> Vector -> Float -> Object {}
object pos vel rad =
    { pos = pos, vel = vel, rad = rad }

obstacleGenerator : Random.Generator Obstacle
obstacleGenerator =
    let
        { width, height } = params.window
        halfHeight = (toFloat height) / 2
        padding = params.maxObstacleRadius
        spawnX = ((toFloat width) / 2) + padding
    in
        Random.map3
            object
            (Vector.random (spawnX, -1 * halfHeight) (spawnX, halfHeight))
            (Vector.random (-1000, -50) (-200, 50))
            (Random.float 5 params.maxObstacleRadius)

initModel : Model
initModel =
    { kirby = { pos = (0, 0), vel = (0, 0), rad = 24, collision = False }
    , obstacles = []
    , input = { up = 0, down = 0, left = 0, right = 0 }
    , health = 10
    }

init : Flags -> (Model, Cmd Msg)
init () = (initModel, Cmd.none)


-- Update

type Msg
    = Frame Float
    | Change Float String
    | SpawnObstacle
    | AddObstacle Obstacle

onScreen : Object a -> Bool
onScreen { pos, rad } =
    let
        (x, y) = pos
        padding = params.maxObstacleRadius
        { width, height } = params.window
        threshX = ((toFloat width) / 2) + padding
        threshY = ((toFloat height) / 2) + padding
    in
        x <= threshX && x >= -threshX && y <= threshY && y >= -threshY

collision : Object a -> Object b -> Bool
collision obj1 obj2 =
    obj1.rad + obj2.rad > Vector.dist obj1.pos obj2.pos

checkCollision : List Obstacle -> Kirby -> Kirby
checkCollision obs kirby =
    { kirby | collision = List.any (collision kirby) obs }

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
                        |> checkCollision model.obstacles
                obs =
                    model.obstacles
                        |> List.filter onScreen
                        |> List.map (position dt)
                health =
                    if not model.kirby.collision && kirby.collision
                        then model.health - 1
                    else
                        model.health
            in
                ( { model
                    | kirby = kirby
                    , obstacles = obs
                    , health = health
                  }
                , Cmd.none
                )
        SpawnObstacle ->
            ( model
            , Random.generate
                AddObstacle obstacleGenerator
            )
        AddObstacle obs ->
            ( { model | obstacles = obs :: model.obstacles }
            , Cmd.none
            )
        Change status key ->
            ( { model | input = setInput status key model.input }
            , Cmd.none
            )


-- View

kirbyImage : Input -> String
kirbyImage { up, down, left, right } =
    let
        vert = up - down
        horiz = right - left
        dir =
            if vert == 1
                then "up"
            else if vert == -1
                then "down"
            else if horiz == 1
                then "right"
            {--else if horiz == -1
                then "left"--}
            else
                "still"
    in
        "assets/images/kirby/" ++ dir ++ ".gif"

display : Model -> Collage Msg
display model =
    let
        img =
            image (49, 42) (kirbyImage model.input)
                |> shift model.kirby.pos
        obs = List.map (\{ pos, rad } -> circle rad |> filled (uniform Color.red) |> shift pos) model.obstacles
        rect =
            rectangle 1920 1080
                |> filled (uniform Color.black)
    in
        rect
        |> impose (group obs)
        |> impose img


view : Model -> Browser.Document Msg
view model =
    let
        active = [ svg (display model)
                 ,  Html.h1
                    []
                    [ Html.text <| "Health: " ++ Debug.toString model.health ]
                 ]
        over = [ Html.h1
                    []
                    [ Html.text "Game Over" ]
               ]
    in
        { title = "Kirby Racer"
        , body = if model.health > 0 then active else over
        }


-- Subscriptions

keyDecoder : (String -> Msg) -> Decode.Decoder Msg
keyDecoder action =
    Decode.map action (Decode.field "key" Decode.string)

subscriptions : Model -> Sub Msg
subscriptions model =
    if model.health > 0 then
        Sub.batch
            [ Browser.Events.onKeyUp <| keyDecoder <| Change 0
            , Browser.Events.onKeyDown <| keyDecoder <| Change 1
            , Browser.Events.onAnimationFrameDelta Frame
            , Time.every params.obstacleSpawnTime (always SpawnObstacle)
            ]
    else
        Sub.none


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