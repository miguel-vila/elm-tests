import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import TodoStyles exposing (styles)
import Json.Decode as Json
import Html.Events exposing (on, keyCode, onInput, onCheck, onClick)

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

update : Msg -> Model -> Model
update msg model =
    case msg of
        Add todo ->
            let nextId = model.currentId + 1
            in { model
                   | currentId = nextId
                   , todos = todo :: model.todos
                   , todo = initialTodo nextId
               }
        Complete todo ->
            let modifyTodo td = if td.id == todo.id then completeTodo todo else td
            in { model
                   | todos = List.map modifyTodo model.todos
               }
        Delete todo ->
            { model
                | todos = List.filter (\td -> td.id /= todo.id) model.todos
            }
        Filter filterState ->
            { model
                | filter = filterState
            }
        SetText todoText ->
            let todo = model.todo
                updatedTodo = { todo | title = todoText }
            in { model
                   | todo = updatedTodo
               }
        ClearCompleted ->
            { model
                | todos = clearCompleted model.todos
            }

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

                   
main =
      Html.beginnerProgram
          { model = initialModel
          , update = update
          , view = view
          }
