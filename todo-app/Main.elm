port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import TodoStyles exposing (styles)
import Json.Decode as Json
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)

import Json.Decode exposing ((:=))
import Json.Encode


type alias Todo =
    { id        : Int
    , title     : String
    , completed : Bool
    , editing   : Bool
    }

type FilterState = All | Active | Completed

type alias Model =
    { currentId : Int
    , todos     : List Todo
    , todo      : Todo
    , filter    : FilterState
    }

type Msg
    = Add Todo
    | Complete Todo
    | Delete Todo
    | Filter FilterState
    | SetText String
    | ClearCompleted
    | Set Model
    | NoOp

initialTodo id = { id = id
                 , title = ""
                 , completed = False
                 , editing = False
                 }
      
initialModel =
    { currentId = 0
    , todos = [
          ]
    , todo = initialTodo 0
    , filter = All
    }

completeTodo : Todo -> Todo
completeTodo todo = { todo | completed = not todo.completed }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Add todo ->
            let nextId = model.currentId + 1
                newModel = { model
                               | currentId = nextId
                               , todos = todo :: model.todos
                               , todo = initialTodo nextId
                           }
            in (newModel, sendToStorage newModel)
        Complete todo ->
            let modifyTodo td = if td.id == todo.id then completeTodo todo else td
                newModel = { model
                               | todos = List.map modifyTodo model.todos
                           }
            in (newModel, sendToStorage newModel)
        Delete todo ->
            let newModel = { model
                               | todos = List.filter (\td -> td.id /= todo.id) model.todos
                           }
            in (newModel, sendToStorage newModel)
        Filter filterState ->
            let newModel = { model
                               | filter = filterState
                           }
            in (newModel, sendToStorage newModel)
        SetText todoText ->
            let todo = model.todo
                updatedTodo = { todo | title = todoText }
                newModel = { model
                               | todo = updatedTodo
                           }
            in (newModel, sendToStorage newModel)
        ClearCompleted ->
            let newModel = { model
                           | todos = clearCompleted model.todos
                       }
            in (newModel, sendToStorage newModel)
        Set newModel ->
            (newModel, Cmd.none)
        NoOp ->
            (model, Cmd.none)

clearCompleted : List Todo -> List Todo
clearCompleted = List.filter (\todo -> not todo.completed)
    
handleKeyPress : Todo -> Json.Decoder Msg
handleKeyPress todo =
    Json.map (always (Add todo)) (Json.customDecoder keyCode is13)

is13 : Int -> Result String ()
is13 code = if code == 13 then Ok () else Err "not the right key code"
        
todoView : Todo -> Html Msg
todoView todo =
    -- We will give the li the class "completed" if the todo is completed
    li [classList [ ("completed", todo.completed) ] ]
        [ div [class "view"]
              -- We will check the checkbox if the todo is completed
              [ input [ class "toggle"
                      , type' "checkbox"
                      , checked todo.completed
                      , onCheck (always <| Complete todo)
                      ] []
              -- We will use the todo's title as the label text
              , label [] [text todo.title]
              , button [ class "destroy"
                       , onClick (Delete todo)
                       ] []
              ]
        ]

applyFilter : FilterState -> List Todo -> List Todo
applyFilter filter =
    case filter of
        All -> identity
        Active -> List.filter (\todo -> not todo.completed)
        Completed -> List.filter (\todo -> todo.completed)


filterView : Model -> FilterState -> Html Msg
filterView model filter =
    li []
        [ a [ classList [("selected", model.filter == filter)]
            , href "#"
            , onClick (Filter filter)] [text (toString filter)] ]

countLeftTodos : Model -> Int
countLeftTodos {todos} = todos |> List.filter (\todo -> not todo.completed)
                               |> List.length
            
view : Model -> Html Msg
view model =
    div []
        [ node "style" [type' "text/css"] [ text styles ]
        , section [class "todoapp"]
            [ header [class "header"]
                  [ h1 [] [text "todos"]
                  , input [class "new-todo"
                          , placeholder "What needs to be done?"
                          , on "keypress" (handleKeyPress model.todo)
                          , onInput SetText
                          , autofocus True
                          , value model.todo.title
                          ] []
                  ]
            , section [class "main"]
                  [ ul [class "todo-list"]
                    (let todos = model.todos |> applyFilter model.filter
                                             |> List.map todoView
                    in todos)
                  ]
            , footer [class "footer"]
                [ span [class "todo-count"]
                      [ strong [] [ text (toString (countLeftTodos model)) ]
                      , text " items left"
                      ]
                , ul [class "filters"]
                    (List.map (filterView model) [ All, Active, Completed ])
                , button [ class "clear-completed"
                         , onClick ClearCompleted
                         ] [text "Clear completed"]
                ]
            ]
        ]

encodeTodo : Todo -> Json.Encode.Value
encodeTodo todo =
    Json.Encode.object
        [ ("id", Json.Encode.int todo.id)
        , ("title", Json.Encode.string todo.title)
        , ("completed", Json.Encode.bool todo.completed)
        , ("editing", Json.Encode.bool todo.editing)
        ]

encodeFilter : FilterState -> Json.Encode.Value
encodeFilter filter = Json.Encode.string (toString filter)

decodeModel : Json.Decode.Value -> Result String Model
decodeModel json =
    Json.Decode.decodeValue modelDecoder json

modelDecoder : Json.Decode.Decoder Model
modelDecoder =
    Json.Decode.object4 Model
        ("currentId" := Json.Decode.int)
        ("todos"     := Json.Decode.list todoDecoder)
        ("todo"      := todoDecoder)
        ("filter"    := filterDecoder)

todoDecoder : Json.Decode.Decoder Todo
todoDecoder =
    Json.Decode.object4 Todo
        ("id" := Json.Decode.int)
        ("title" := Json.Decode.string)
        ("completed" := Json.Decode.bool)
        ("editing" := Json.Decode.bool)
            
filterDecoder : Json.Decode.Decoder FilterState
filterDecoder =
    let decodeToFilterState string =
            case string of
                "All" -> Result.Ok All
                "Active" -> Result.Ok Active
                "Completed" -> Result.Ok Completed
                _ -> Result.Err ("Not a valid filterState: " ++ string)
    in Json.Decode.customDecoder Json.Decode.string decodeToFilterState
        
mapStorageInput : Json.Decode.Value -> Msg
mapStorageInput modelJson =
    case decodeModel modelJson of
        Result.Ok model -> Set model
        Result.Err _ -> NoOp

sendToStorage : Model -> Cmd Msg
sendToStorage model =
    encodeJson model |> storage
                      
encodeJson : Model -> Json.Encode.Value
encodeJson model =
    Json.Encode.object
        [ ("currentId", Json.Encode.int model.currentId)
        , ("todos", Json.Encode.list (List.map encodeTodo model.todos))
        , ("todo", encodeTodo model.todo)
        , ("filter", encodeFilter model.filter)
        ]
                   
main =
      Html.program
          { init = (initialModel, Cmd.none)
          , update = update
          , view = view
          , subscriptions = subscriptions
          }

subscriptions model =
    storageInput mapStorageInput

port storageInput : (Json.Decode.Value -> msg) -> Sub msg

port storage : Json.Encode.Value -> Cmd msg
