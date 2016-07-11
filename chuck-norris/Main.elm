port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)
import Task
import Http
import Json.Decode exposing (at, object2, (:=), string, Decoder, decodeValue)
import Json.Encode


type alias Joke =
    { icon : String
    , text : String
    }


type Model
    = NoJoke
    | Loading
    | Errored
    | SettedJoke Joke


init =
    ( NoJoke, Cmd.none )


type Msg
    = GetJoke
    | Error
    | SetJoke Joke


decodeJoke : Decoder Joke
decodeJoke =
    object2 Joke
        ("icon_url" := string)
        ("value" := string)


url =
    "https://api.chucknorris.io/jokes/random"


getJoke : Cmd Msg
getJoke =
    Task.perform
        (always Error)
        SetJoke
        (Http.get decodeJoke url)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg _ =
    case msg of
        GetJoke ->
            ( Loading, getJoke )

        Error ->
            ( Errored, Cmd.none )

        SetJoke joke ->
            ( SettedJoke joke, Cmd.none )


(=>) =
    (,)


imgStyle : String -> Attribute msg
imgStyle url =
    style
        [ "display" => "inline-block"
        , "width" => "200px"
        , "height" => "200px"
        , "background-position" => "center center"
        , "background-size" => "cover"
        , "background-image" => ("url('" ++ url ++ "')")
        ]


getJokeButton =
    button [ onClick GetJoke ] [ text "Get Chuck Norris Fact!" ]


view : Model -> Html Msg
view model =
    case model of
        NoJoke ->
            div [] [ getJokeButton ]

        Loading ->
            div [ imgStyle "assets/waiting.gif" ] [ text "loading..." ]

        Errored ->
            div [] [ text "there was an error, try again:", getJokeButton ]

        SettedJoke joke ->
            div [] [ div [ imgStyle joke.icon ] [], text joke.text, getJokeButton ]


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = (always Sub.none)
        }
