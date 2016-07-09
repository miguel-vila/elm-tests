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
import AnimationFrame

type alias ColorInfo =
    { color: Color
    , name : String
    }

redColor = ColorInfo red "Red"
blueColor = ColorInfo blue "Blue"
greenColor = ColorInfo green "Green"
yellowColor = ColorInfo yellow "Yellow"

colors : List ColorInfo
colors = [ redColor
         , blueColor
         , greenColor
         , yellowColor
         ]

type alias Point = (Int, Int)

type alias Model =
    { points : List Point
    , x : Int
    , y : Int
    , keyboardModel : Keyboard.Extra.Model
    , clock : Time
    , animation : Animation
    , animations : List (Time -> Animation)
    , color : Color
    }

type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | Tick Time
    | Shake
    | SetColor Color

shakeAnimation : Time -> Animation
shakeAnimation t =
    animation t
        |> from 0
        |> to 40
        |> duration (500*Time.millisecond)
           
shakeAnimation' : Time -> Animation
shakeAnimation' t =
    animation t
        |> from 40
        |> to -20
        |> duration (500*Time.millisecond)
           
shakeAnimation'' : Time -> Animation
shakeAnimation'' t =
    animation t
        |> from -20
        |> to 10
        |> duration (500*Time.millisecond)
           
shakeAnimation''' : Time -> Animation
shakeAnimation''' t =
    animation t
        |> from 10
        |> to 0
        |> duration (500*Time.millisecond)
           
           
animations : List (Time -> Animation)
animations =
    [ shakeAnimation
    , shakeAnimation'
    , shakeAnimation''
    , shakeAnimation'''
    ]

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
          , animation = static 0
          , animations = []
          , color = red
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SetColor color ->
            { model | color = color } ! []
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
                (newPoints, newAnimation, newAnimations) =
                    if isDone model.clock model.animation
                    then
                        let nextAnimation = model.animations
                                                         |> List.head
                                                         |> Maybe.map (\animation -> animation model.clock)
                                                         |> Maybe.withDefault (static 0)
                            nextAnimations = model.animations
                                                         |> List.tail
                                                         |> Maybe.withDefault []
                            justFinished =
                                nextAnimation `equals`(static 0) && not(model.animation `equals` (static 0))
                            nextPoints = if justFinished then [] else model.points
                        in (nextPoints, nextAnimation, nextAnimations)
                    else (model.points, model.animation, model.animations)
                newPoints' =
                    case (x, y) of
                        (0,0) ->
                            newPoints
                        _     ->
                            (newX, newY) :: newPoints
                model' =
                    { model
                        | points = newPoints'
                        , clock = newClock
                        , animation = newAnimation
                        , animations = newAnimations  
                    }
            in
                case (x,y) of
                    (0,0) -> model' ! []
                    _     -> { model'
                                 | x = newX
                                 , y = newY
                             } ! []
        Shake ->
            { model
                | animation = shakeAnimation model.clock
                , animations = animations
            } ![]
                       
        
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
            ([ collage 800 800
                  [ (rotate (degrees angle) (drawLine model.color model.points)) ]
            |> Element.toHtml
            , shakeButton
            ] ++ (List.map colorButton colors))
           
drawLine : Color -> List Point -> Form
drawLine color points =
    let intsToFloats : (Int, Int) -> (Float, Float)
        intsToFloats (x, y) = (toFloat x, toFloat y)
        shape = path (List.map intsToFloats points)
    in
        shape
            |> traced (solid color)

colorButton : ColorInfo -> Html Msg
colorButton {color, name} =
    button [onClick (SetColor color)] [Html.text name]

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
        , AnimationFrame.diffs Tick
        ]
