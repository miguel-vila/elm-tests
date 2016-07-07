module Stamps exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.App as Html
import Mouse
import Keyboard

type Shape = Pentagon
           | Circle

type alias Position = (Int, Int)

type alias Stamp =
    { position : Position
    , shape    : Shape
    }

type alias Model =
    { stamps : List Stamp
    , shape  : Shape
    }

type Msg
    = Click Position
    | HandleShift Shape
    | Clear
    | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Click pos -> { model | stamps = { shape = model.shape , position = pos} :: model.stamps } ! []
        HandleShift shape -> { model | shape = shape } ! []
        Clear -> { model | stamps = [] } ! []
        NoOp -> model ! []

drawStamp : Stamp -> Form
drawStamp { shape, position } =
    let (x,y) = position
        theShape = case shape of
                       Pentagon -> ngon 5 50
                       Circle -> circle 50
    in theShape
        |> filled red
        |> move (toFloat x, toFloat(-1*y))

view : Model -> Html Msg
view {stamps} =
    let theGroup =
            group (List.map drawStamp stamps)
        originGroup =
            move (-400,400) theGroup
    in collage 800 800 [ originGroup ] |> Element.toHtml

stamps : List (Int,Int)
stamps = [(0,0), (100,100), (200,100)]
        
main : Program Never
main = Html.program
       { init = ({stamps = [], shape = Pentagon}, Cmd.none)
       , update = update
       , view = view
       , subscriptions = subs
       }

mapKeyDown : Int -> Msg
mapKeyDown keyCode =
    case Debug.log "mapKeyD" keyCode of
        40 -> HandleShift Circle
        _  -> NoOp
    
mapKeyUp : Int -> Msg
mapKeyUp keyCode =
    case Debug.log "mapKeyUp" keyCode of
        38 -> HandleShift Pentagon
        67 -> Clear
        _  -> NoOp

subs model =
    Sub.batch
        [ Mouse.clicks (\{x,y} -> Click (x,y))
        , Keyboard.downs mapKeyDown
        , Keyboard.ups mapKeyUp
        ]
