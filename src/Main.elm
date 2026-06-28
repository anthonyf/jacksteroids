module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Svg exposing (Svg, rect, svg)
import Svg.Attributes exposing (fill, height, viewBox, width)


type alias Model =
    { elapsedMs : Float
    }


type Msg
    = Tick Float


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
    ( { elapsedMs = 0 }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick deltaMs ->
            ( { model | elapsedMs = model.elapsedMs + deltaMs }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Browser.Events.onAnimationFrameDelta Tick


view : Model -> Html Msg
view _ =
    svg
        [ width "100vw"
        , height "100vh"
        , viewBox "0 0 800 600"
        ]
        [ background ]


background : Svg Msg
background =
    rect
        [ width "800"
        , height "600"
        , fill "#000"
        ]
        []
