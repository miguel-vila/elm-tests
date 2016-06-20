module RandomGif exposing (init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Task
import Http
import Json.Decode as Json

type alias Model =
    { topic: String
    , gifUrl: String
    }

waitingGif = "assets/waiting.gif"
    
init : String -> (Model, Cmd Msg)
init topic =
    ( Model topic waitingGif
    , getRandomGif topic
    )

type Msg
    = RequestMore
    | FetchSucceed String
    | FetchFail


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RequestMore ->
            ( { model | gifUrl = waitingGif }
            , getRandomGif model.topic
            )
        FetchSucceed maybeUrl ->
            ( Model model.topic maybeUrl
            , Cmd.none
            )
        FetchFail ->
            ( model
            , Cmd.none
            )

getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url = randomUrl topic
    in
        Task.perform (\_ -> FetchFail) FetchSucceed (Http.get decodeGifUrl url)

randomUrl : String -> String
randomUrl topic =
    Http.url "http://api.giphy.com/v1/gifs/random"
        [ "api_key" => "dc6zaTOxFJmzC"
        , "tag" => topic
        ]

decodeGifUrl : Json.Decoder String
decodeGifUrl =
    Json.at ["data", "image_url"] Json.string
            
(=>) = (,)


view : Model -> Html Msg
view model =
    div [ style [ "width" => "200px" ] ]
        [ h2 [headerStyle] [text model.topic]
        , div [imgStyle model.gifUrl] []
        , button [ onClick RequestMore ] [ text "More Please!" ]
        ]


headerStyle : Attribute Msg
headerStyle =
    style
    [ "width" => "200px"
    , "text-align" => "center"
    ]

imgStyle : String -> Attribute Msg
imgStyle url =
    style
    [ "display" => "inline-block"
    , "width" => "200px"
    , "height" => "200px"
    , "background-position" => "center center"
    , "background-size" => "cover"
    , "background-image" => ("url('" ++ url ++ "')")
    ]
