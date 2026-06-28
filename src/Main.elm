module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Json.Decode as Decode
import Svg exposing (Svg, circle, polygon, rect, svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, strokeWidth, transform, viewBox, width)


type alias Model =
    { elapsedMs : Float
    , ship : Ship
    , controls : Controls
    , bullets : List Bullet
    , timeSinceLastShotMs : Float
    }


type alias Ship =
    { position : Vec2
    , velocity : Vec2
    , thrust : Vec2
    , heading : Float
    , angularVelocity : Float
    , collisionBoundary : Float
    }


type alias Vec2 =
    { x : Float
    , y : Float
    }


type alias Bullet =
    { position : Vec2
    , velocity : Vec2
    , ageMs : Float
    }


type alias Controls =
    { thrusting : Bool
    , turningLeft : Bool
    , turningRight : Bool
    , firing : Bool
    }


type Msg
    = Tick Float
    | KeyChanged String Bool


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { elapsedMs = 0
      , ship = initialShip
      , controls = initialControls
      , bullets = []
      , timeSinceLastShotMs = shotCooldownMs
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            let
                ship =
                    updateShip deltaMs model.controls model.ship

                advancedBullets =
                    updateBullets deltaMs model.bullets

                fireResult =
                    updateFiring deltaMs model.controls ship advancedBullets model.timeSinceLastShotMs
            in
            ( { model
                | elapsedMs = model.elapsedMs + deltaMs
                , ship = ship
                , bullets = fireResult.bullets
                , timeSinceLastShotMs = fireResult.timeSinceLastShotMs
              }
            , Cmd.none
            )

        KeyChanged key isPressed ->
            ( { model | controls = updateControls key isPressed model.controls }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onKeyDown (Decode.map (\key -> KeyChanged key True) keyDecoder)
        , Browser.Events.onKeyUp (Decode.map (\key -> KeyChanged key False) keyDecoder)
        ]


view : Model -> Html Msg
view model =
    svg
        [ width "100vw"
        , height "100vh"
        , viewBox "0 0 800 600"
        ]
        [ background
        , Svg.g [] (List.map viewBullet model.bullets)
        , viewShip model.ship
        ]


background : Svg Msg
background =
    rect
        [ width "800"
        , height "600"
        , fill "#000"
        ]
        []


initialShip : Ship
initialShip =
    { position = { x = 400, y = 300 }
    , velocity = zero
    , thrust = zero
    , heading = 0
    , angularVelocity = 0
    , collisionBoundary = 18
    }


initialControls : Controls
initialControls =
    { thrusting = False
    , turningLeft = False
    , turningRight = False
    , firing = False
    }


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string


updateControls : String -> Bool -> Controls -> Controls
updateControls key isPressed controls =
    case key of
        "ArrowUp" ->
            { controls | thrusting = isPressed }

        "ArrowLeft" ->
            { controls | turningLeft = isPressed }

        "ArrowRight" ->
            { controls | turningRight = isPressed }

        " " ->
            { controls | firing = isPressed }

        "Spacebar" ->
            { controls | firing = isPressed }

        "Space" ->
            { controls | firing = isPressed }

        _ ->
            controls


updateShip : Float -> Controls -> Ship -> Ship
updateShip deltaMs controls ship =
    let
        deltaSeconds =
            deltaMs / 1000

        angularVelocity =
            rotationFromControls controls

        heading =
            ship.heading + angularVelocity * deltaSeconds

        thrust =
            if controls.thrusting then
                thrustFromHeading heading

            else
                zero

        velocity =
            add ship.velocity (scale deltaSeconds thrust)

        position =
            wrapPosition (add ship.position (scale deltaSeconds velocity))
    in
    { ship
        | position = position
        , velocity = velocity
        , thrust = thrust
        , heading = heading
        , angularVelocity = angularVelocity
    }


rotationFromControls : Controls -> Float
rotationFromControls controls =
    case ( controls.turningLeft, controls.turningRight ) of
        ( True, False ) ->
            -turnSpeed

        ( False, True ) ->
            turnSpeed

        _ ->
            0


thrustFromHeading : Float -> Vec2
thrustFromHeading heading =
    { x = sin heading * thrustPower
    , y = -(cos heading) * thrustPower
    }


updateBullets : Float -> List Bullet -> List Bullet
updateBullets deltaMs bullets =
    let
        deltaSeconds =
            deltaMs / 1000

        updateBullet bullet =
            { bullet
                | position = wrapPosition (add bullet.position (scale deltaSeconds bullet.velocity))
                , ageMs = bullet.ageMs + deltaMs
            }
    in
    bullets
        |> List.map updateBullet
        |> List.filter (\bullet -> bullet.ageMs < bulletTimeToLiveMs)


updateFiring : Float -> Controls -> Ship -> List Bullet -> Float -> { bullets : List Bullet, timeSinceLastShotMs : Float }
updateFiring deltaMs controls ship bullets timeSinceLastShotMs =
    if controls.firing then
        let
            availableShotTimeMs =
                timeSinceLastShotMs + deltaMs

            shotsToFire =
                floor (availableShotTimeMs / shotCooldownMs)

            remainingShotTimeMs =
                availableShotTimeMs - toFloat shotsToFire * shotCooldownMs
        in
        { bullets = bullets ++ List.repeat shotsToFire (createBullet ship)
        , timeSinceLastShotMs = remainingShotTimeMs
        }

    else
        { bullets = bullets
        , timeSinceLastShotMs = shotCooldownMs
        }


createBullet : Ship -> Bullet
createBullet ship =
    let
        direction =
            directionFromHeading ship.heading

        position =
            wrapPosition (add ship.position (scale bulletSpawnOffset direction))

        velocity =
            add ship.velocity (scale bulletSpeed direction)
    in
    { position = position
    , velocity = velocity
    , ageMs = 0
    }


directionFromHeading : Float -> Vec2
directionFromHeading heading =
    { x = sin heading
    , y = -(cos heading)
    }


viewShip : Ship -> Svg Msg
viewShip ship =
    polygon
        [ points "0,-24 13,14 -13,14"
        , fill "#000"
        , stroke "#fff"
        , strokeWidth "2"
        , transform
            ("translate("
                ++ String.fromFloat ship.position.x
                ++ " "
                ++ String.fromFloat ship.position.y
                ++ ") rotate("
                ++ String.fromFloat (radiansToDegrees ship.heading)
                ++ ")"
            )
        ]
        []


viewBullet : Bullet -> Svg Msg
viewBullet bullet =
    circle
        [ cx (String.fromFloat bullet.position.x)
        , cy (String.fromFloat bullet.position.y)
        , r (String.fromFloat bulletRadius)
        , fill "#fff"
        ]
        []


wrapPosition : Vec2 -> Vec2
wrapPosition position =
    { x = wrap 0 playfieldWidth position.x
    , y = wrap 0 playfieldHeight position.y
    }


wrap : Float -> Float -> Float -> Float
wrap lower upper value =
    if value < lower then
        upper

    else if value > upper then
        lower

    else
        value


add : Vec2 -> Vec2 -> Vec2
add a b =
    { x = a.x + b.x
    , y = a.y + b.y
    }


scale : Float -> Vec2 -> Vec2
scale scalar vector =
    { x = scalar * vector.x
    , y = scalar * vector.y
    }


radiansToDegrees : Float -> Float
radiansToDegrees radians =
    radians * 180 / pi


zero : Vec2
zero =
    { x = 0, y = 0 }


playfieldWidth : Float
playfieldWidth =
    800


playfieldHeight : Float
playfieldHeight =
    600


thrustPower : Float
thrustPower =
    260


turnSpeed : Float
turnSpeed =
    4


bulletsPerSecond : Float
bulletsPerSecond =
    7


shotCooldownMs : Float
shotCooldownMs =
    1000 / bulletsPerSecond


bulletTimeToLiveMs : Float
bulletTimeToLiveMs =
    900


bulletSpeed : Float
bulletSpeed =
    430


bulletRadius : Float
bulletRadius =
    2


bulletSpawnOffset : Float
bulletSpawnOffset =
    24
