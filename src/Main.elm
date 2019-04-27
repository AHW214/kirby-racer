module Main exposing (..)

-- Imports

import Vector
import Object
import Kirby exposing (Kirby)
import Obstacle exposing (Obstacle)
import Laser exposing (Laser)
import Input exposing (Input)

import Browser
import Browser.Events
import Time
import Random
import Json.Decode as Decode
import Html exposing (Html, div, h1, text)
import Html.Attributes
import Svg exposing (svg, rect, image)
import Svg.Attributes exposing (x, y, viewBox, fill, width, height)
import Svg exposing (Svg)


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


-- Model

type alias Model =
    { kirby : Kirby
    , obstacles : List Obstacle
    , lasers : List Laser
    , keys : Input
    }

init : Flags -> ( Model, Cmd Msg )
init () =
    ( { kirby = Kirby.init
      , obstacles = []
      , lasers = []
      , keys = Input.init
      }
    , Cmd.none
    )


-- Update

type Msg
    = Frame Float
    | KeyChange Bool String
    | SpawnObstacle
    | AddObstacle Obstacle

update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ keys, kirby, obstacles, lasers } as model) =
    case msg of
        Frame ms ->
            let
                dt =
                    ms / 1000

                newLasers =
                    lasers ++ fireLaser kirby

                newKirby =
                    { kirby
                        | acc = Vector.scale Kirby.speed <| Input.toDir keys
                        , collision = List.any (Object.isColliding kirby) obstacles
                        , chargeTime = if keys.space then kirby.chargeTime + dt else 0
                    }
            in
                ( { model
                     | kirby = Kirby.tick dt newKirby
                     , obstacles = Obstacle.tickAll dt obstacles
                     , lasers = Laser.tickAll dt newLasers
                  }
                , Cmd.none
                )

        SpawnObstacle ->
            ( model
            , Random.generate
                AddObstacle Obstacle.generator
            )

        AddObstacle obs ->
            ( { model | obstacles = obs :: obstacles }
            , Cmd.none
            )

        KeyChange status key ->
            let
                newKeys =
                    Input.set status key keys

                newKirby =
                    { kirby | firing = keys.space && not newKeys.space }
            in
                ( { model | keys = newKeys, kirby = newKirby }
                , Cmd.none
                )


fireLaser : Kirby -> List Laser
fireLaser { firing, chargeTime, pos, vel } =
    if firing then
        [Laser.fire chargeTime pos vel]
    else
        []


-- Subscriptions

keyDecoder : (String -> Msg) -> Decode.Decoder Msg
keyDecoder action =
    Decode.map action (Decode.field "key" Decode.string)

subscriptions : Model -> Sub Msg
subscriptions { kirby } =
    if kirby.hp > 0 then
        Sub.batch
            [ Browser.Events.onKeyUp <| keyDecoder <| KeyChange False
            , Browser.Events.onKeyDown <| keyDecoder <| KeyChange True
            , Browser.Events.onAnimationFrameDelta Frame
            , Time.every 100 (always SpawnObstacle) -- dont hardcode
            ]
    else
        Sub.none


-- View

view : Model -> Browser.Document Msg
view ({ kirby } as model) =
    { title = "Kirby Racer"
    , body = [ if kirby.hp > 0 then gameView model else overView ]
    }

gameView : Model -> Html Msg
gameView { kirby, obstacles, lasers } =
    svg [ viewBox "0 0 1920 1080", width "1920" ]
        [ backgroundView
        , Svg.text_ [fill "#ffffff", x "50%", y "50%"] [Svg.text <| Debug.toString kirby]
        , Kirby.view kirby
        , Obstacle.viewAll obstacles
        , Laser.viewAll lasers
        ]

overView : Html Msg
overView =
    div []
        [ h1 [] [text "Game Over"]
        , backgroundView
        ]

backgroundView : Html Msg
backgroundView =
    rect [ x "0", y "0", width "1920", height "1080", fill "#000000" ] []


