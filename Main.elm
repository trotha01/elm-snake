module Main exposing (..)

import Browser exposing (..)
import Browser.Dom exposing (..)
import Browser.Events exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Math.Vector2 as Vec2 exposing (..)
import Random as Rand exposing (..)
import Task


main =
    Browser.document
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { timeElapsed : Float
    , viewport : Viewport
    , seed : Rand.Seed
    , pause : Bool
    , snake : Snake
    , food : List Food
    }


type alias Snake =
    { position : Vec2
    , velocity : Vec2
    , radius : Float
    , tail : Int
    }


type alias Food =
    { id : Float -- we are using timestamp as id for now
    , position : Vec2
    , radius : Float
    }


type alias Positioned a =
    { a | position : Vec2 }


type alias Circle a =
    Positioned { a | radius : Float }


type alias Flags =
    {}


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { timeElapsed = 0
      , viewport = initViewport
      , seed = Rand.initialSeed 0
      , pause = True
      , snake = initSnake
      , food = []
      }
    , Task.perform UpdateViewport getViewport
    )


initViewport =
    { scene = initScene
    , viewport = initSubViewport
    }


initScene =
    { width = 0, height = 0 }


initSubViewport =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    }


initSnake : Snake
initSnake =
    { position = initPosition
    , velocity = vec2 speed 0
    , radius = 20
    , tail = 1
    }


initFood : Float -> ( Float, Float ) -> Food
initFood id ( x, y ) =
    { id = id
    , position = vec2 x y
    , radius = 20
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
    | UpdateViewport Viewport


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateViewport viewport ->
            ( { model | viewport = viewport }, Cmd.none )

        Tick delta ->
            ( model
                |> updateTimeElapsed delta
                |> updateSnake delta
                |> addFood model.viewport delta
                |> checkCollisions
            , Cmd.none
            )

        ChangeDir dir ->
            ( { model | snake = changeDir dir model.snake }, Cmd.none )

        TogglePause ->
            ( { model | pause = not model.pause }, Cmd.none )


checkCollisions : Model -> Model
checkCollisions model =
    let
        ( newSnake, newFoodList ) =
            List.foldl
                (\food ( snake, uneaten ) ->
                    if circlesCollide snake food then
                        ( { snake | tail = snake.tail + 1 }, uneaten )
                    else
                        ( snake, food :: uneaten )
                )
                ( model.snake, [] )
                model.food
    in
    { model | snake = newSnake, food = newFoodList }


updateTimeElapsed : Float -> Model -> Model
updateTimeElapsed delta model =
    { model | timeElapsed = model.timeElapsed + delta }


addFood : Viewport -> Float -> Model -> Model
addFood viewport delta model =
    let
        ( newFoodList, seed ) =
            case Rand.step (foodGenerator viewport model.timeElapsed) model.seed of
                ( Nothing, newSeed ) ->
                    ( model.food, newSeed )

                ( Just newFood, newSeed ) ->
                    ( newFood :: model.food, newSeed )
    in
    { model | food = newFoodList, seed = seed }


foodGenerator : Viewport -> Float -> Generator (Maybe Food)
foodGenerator viewport id =
    Rand.map3
        (\i x y ->
            if i > 1 then
                Nothing
            else
                Just (initFood id ( x, y ))
        )
        (Rand.int 0 100)
        (Rand.float 20 (viewport.scene.width - 20))
        (Rand.float 20 (viewport.scene.height - 20))


{-| speed is pixels per frame
-}
speed : Float
speed =
    0.1


dirChangeSpeed : Float
dirChangeSpeed =
    0.5


updateSnake : Float -> Model -> Model
updateSnake delta model =
    { model | snake = snakeUpdate delta model.snake }


snakeUpdate : Float -> Snake -> Snake
snakeUpdate delta snake =
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


{-| circlesCollide checks if two circles overlap
-}
circlesCollide : Circle a -> Circle b -> Bool
circlesCollide c1 c2 =
    let
        distSq =
            Vec2.distanceSquared c1.position c2.position

        radSq =
            (c1.radius + c2.radius) ^ 2
    in
    distSq < radSq



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
        , viewFoods model.food
        ]


viewFoods : List Food -> Html Msg
viewFoods foods =
    div [] (List.map viewFood foods)


viewFood : Food -> Html Msg
viewFood food =
    div
        [ style "position" "absolute"
        , style "left" (px (getX food.position))
        , style "top" (px (getY food.position))
        ]
        [ text "food" ]


viewSnake : Snake -> Html Msg
viewSnake snake =
    div
        [ style "position" "absolute"
        , style "left" (px (getX snake.position))
        , style "top" (px (getY snake.position))
        , style "transform" ("rotate(" ++ getDirectionRad snake ++ ")")
        ]
        [ text ("sn" ++ String.repeat snake.tail "a" ++ "ke")
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
