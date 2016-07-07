import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Html exposing (..)
import Html.App as App
import Keyboard.Extra
import Time exposing (Time, second)
import Animation exposing (..)
import Html.Events exposing (onClick)

type alias Point = (Int, Int)

type alias Model =
    { points : List Point
    , x : Int
    , y : Int
    , keyboardModel : Keyboard.Extra.Model
    , clock : Time
    , animation : Animation
    }

type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | Tick Time
    | Shake

shakeAnimation : Time -> Animation
shakeAnimation t =
    animation t
        |> from 0
        |> to 360
        |> duration (4 * Time.second)

init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) = Keyboard.Extra.init
    in
        ( { points = [(0, 0)]
          , x = 0
          , y = 0
          , keyboardModel = keyboardModel
          , clock = 0
          , animation = shakeAnimation 0
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.Extra.update keyMsg model.keyboardModel
            in
                ( { model | keyboardModel = keyboardModel }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )
        Tick dt ->
            let
                { x , y } = Keyboard.Extra.arrows model.keyboardModel
                newX = model.x + x
                newY = model.y + y
                newClock = model.clock + dt
            in
                case (x,y) of
                    (0,0) -> { model | clock = newClock } ! []
                    _     -> { model
                                 | points = (newX, newY) :: model.points
                                 , x = newX
                                 , y = newY
                                 , clock = newClock
                             } ! []
        Shake ->
            { model | points = [] } ![]
                       
        
keyUp : Keyboard.KeyCode -> Model -> Model
keyUp keyCode model =
    case keyCode of
        38 -> -- up
             { model | y = model.y + 1, points = (model.x, model.y + 1) :: model.points }
        40 -> -- down
             { model | y = model.y - 1, points = (model.x, model.y - 1) :: model.points }
        37 -> -- left
             { model | x = model.x - 1, points = (model.x - 1, model.y) :: model.points }
        39 -> -- right
             { model | x = model.x + 1, points = (model.x + 1, model.y) :: model.points }
        _ -> model

shakeButton =
      Html.button [onClick Shake] [ Html.text "Shake it good" ]
    
view : Model -> Html Msg
view model =
    let
        angle =
            animate model.clock model.animation
    in
        div []
            [ collage 800 800
                  [ (rotate (degrees angle) (drawLine model.points)) ]
            |> Element.toHtml
            , shakeButton
            ]
           
drawLine : List Point -> Form
drawLine points =
    let intsToFloats : (Int, Int) -> (Float, Float)
        intsToFloats (x, y) = (toFloat x, toFloat y)
        shape = path (List.map intsToFloats points)
    in
        shape
            |> traced (solid red)

main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , Time.every (1/30 * second) Tick
        ]
