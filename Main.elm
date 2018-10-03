module Main exposing (..)

import Browser exposing (..)
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Math.Vector2 as Vec2 exposing (..)


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { pause : Bool
    , snake : Snake
    }


type alias Snake =
    { position : Vec2
    , velocity : Vec2
    , direction : Float -- polar coords, 0 to 2pi
    }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { pause = True, snake = initSnake }, Cmd.none )


initSnake : Snake
initSnake =
    { position = initPosition
    , velocity = vec2 1 0
    , direction = 0
    }


initPosition =
    vec2 50 50



-- UPDATE


type Direction
    = Left
    | Right
    | Other


type Msg
    = Tick Float
    | ChangeDir Direction
    | TogglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick delta ->
            ( { model | snake = updateSnake delta model.snake }, Cmd.none )

        ChangeDir dir ->
            ( { model | snake = changeDir dir model.snake }, Cmd.none )

        TogglePause ->
            ( { model | pause = not model.pause }, Cmd.none )


{-| speed is pixels per frame
-}
speed : Float
speed =
    0.1


dirChangeSpeed : Float
dirChangeSpeed =
    0.5


updateSnake : Float -> Snake -> Snake
updateSnake delta snake =
    { snake | position = move delta snake.velocity snake.position }


move : Float -> Vec2 -> Vec2 -> Vec2
move delta velocity position =
    Vec2.add position (Vec2.scale delta velocity)


changeDir : Direction -> Snake -> Snake
changeDir direction snake =
    { snake | velocity = moveDirection direction snake.velocity }


moveDirection : Direction -> Vec2 -> Vec2
moveDirection direction vector =
    let
        currentAngle =
            atan2 (getY vector) (getX vector)

        magnitude =
            speed

        newAngle =
            case direction of
                Left ->
                    currentAngle + dirChangeSpeed

                Right ->
                    currentAngle - dirChangeSpeed

                Other ->
                    currentAngle

        newX =
            magnitude * cos newAngle

        newY =
            magnitude * sin newAngle
    in
    vec2 newX newY



-- VIEW


view : Model -> Document Msg
view model =
    { title = title
    , body = [ viewGame model ]
    }


title : String
title =
    "Snake"


viewGame : Model -> Html Msg
viewGame model =
    div []
        [ viewSnake model.snake
        ]


viewSnake : Snake -> Html Msg
viewSnake snake =
    div
        [ style "position" "absolute"
        , style "left" (px (getX snake.position))
        , style "top" (px (getY snake.position))
        , style "transform" ("rotate(" ++ getDirectionRad snake ++ ")")
        ]
        [ text "snake"
        ]


getDirectionRad : Snake -> String
getDirectionRad snake =
    String.fromFloat (getDirection snake) ++ "rad"


getDirection : Snake -> Float
getDirection snake =
    atan2 (getY snake.velocity) (getX snake.velocity)


px : Float -> String
px x =
    String.fromInt (round x) ++ "px"



-- SUBSCRIPTIONS


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Msg
toDirection string =
    case string of
        "ArrowLeft" ->
            ChangeDir Left

        "ArrowRight" ->
            ChangeDir Right

        _ ->
            TogglePause


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.pause then
        onKeyDown keyDecoder
    else
        Sub.batch
            [ onAnimationFrameDelta Tick
            , onKeyDown keyDecoder
            ]