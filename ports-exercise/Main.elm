port module Main exposing (..)

import Html exposing (..)
import Html.App as Html
import Html.Events exposing (onClick)

type alias Model = Int

type Msg = Inc
         | Send
         | NoOp
    
update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Inc  -> (model+1, Cmd.none)
        Send -> (model  , elmMsgs model)
        NoOp -> (model  , Cmd.none)

view : Model -> Html Msg
view model =
    div []
        [ span []
              [ text ("value :" ++ toString model) ]
        , button [ onClick Send ]
            [ text "Send to JS"]
        ]

main = Html.program
       { init = (0,Cmd.none)
       , update = update
       , view = view
       , subscriptions = subscriptions
       }

port jsMsgs : (Int -> msg) -> Sub msg
    
subscriptions model =
    jsMsgs mapJsMsgs

mapJsMsgs : Int -> Msg
mapJsMsgs n =
    case n of
        1 -> Inc
        _ -> NoOp
    
port elmMsgs : Int -> Cmd msg
