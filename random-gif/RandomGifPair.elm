module RandomGifPair exposing (init, update, view)

import RandomGif
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html

type alias Model =
    { left   : RandomGif.Model
    , right  : RandomGif.Model
    }

type Msg
    = Left  RandomGif.Msg
    | Right RandomGif.Msg

init : String -> String -> (Model, Cmd Msg)
init leftTopic rightTopic =
    let
        (left, leftCmd) = RandomGif.init leftTopic
        (right, rightCmd) = RandomGif.init rightTopic
    in ( { left = left
         , right = right
         }
       , Cmd.batch
           [ Cmd.map Left leftCmd
           , Cmd.map Right rightCmd
           ]
       )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Left leftMsg ->
            let (left, cmd) = RandomGif.update leftMsg model.left
            in ( { model | left = left }
               , Cmd.map Left cmd
               )
        Right rightMsg ->
            let (right, cmd) = RandomGif.update rightMsg model.right
            in ( { model | right = right }
               , Cmd.map Right cmd
               )

view : Model -> Html Msg
view model =
    div [ style [ ("display","flex") ] ]
        [ Html.map Left (RandomGif.view model.left)
        , Html.map Right (RandomGif.view model.right)
        ]
