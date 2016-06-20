port module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)

type alias Model = { total: Int
                   , increments: Int
                   , decrements: Int
                   }

type Msg = Increment | Decrement | NoOp

increment model =
    { model | total = model.total + 1 , increments = model.increments + 1 }

decrement model =
    { model | total = model.total - 1 , decrements = model.decrements + 1 }
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Increment -> (increment model, incrementCmd ())
        Decrement -> (decrement model, Cmd.none)
        NoOp      -> (model          , Cmd.none)

view : Model -> Html Msg
view {total, increments, decrements}  =
    div []
        [ button [onClick Decrement] [ text "-" ]
        , h3 [] [text ("Decrements: " ++ (toString decrements))]
        , div [] [text (toString total)]
        , h3 [] [text ("Increments: " ++ (toString increments))]
        , button [onClick Increment] [ text "+" ]
        ]

initialModel = {total = 0, increments = 0, decrements = 0}
        
main =
    Html.program
        { init = (initialModel, Cmd.none)
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

subscriptions model =
    jsMsgs mapJsMsg

port jsMsgs : (Int -> msg) -> Sub msg
              
mapJsMsg : Int -> Msg
mapJsMsg n =
    case n of
        1 -> Increment
        _ -> NoOp

port incrementCmd : () -> Cmd msg
                 
