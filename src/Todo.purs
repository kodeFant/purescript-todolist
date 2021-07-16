module Todo (todoListRoot) where

import Prelude
import Data.Pipe ((|>))
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Data.UUID as UUID
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events as Event
import React.Basic.Hooks ((/\))
import React.Basic.Hooks as React

-- MODEL
type Todo
  = { id :: String
    , description :: String
    , completed :: Boolean
    }

type Model
  = { todos :: Array Todo
    , newTodoInput :: String
    , generatedId :: String
    }

initialModel :: String -> Model
initialModel generatedId =
  { todos: []
  , newTodoInput: ""
  , generatedId: generatedId
  }

-- UPDATE
data TodoAction
  = AddTodo String
  | RemoveTodo Todo
  | UpdateNewTodoInput String
  | UpdateNewTodoId String
  | SetDone Todo

update :: Model -> TodoAction -> Model
update model action = case action of
  AddTodo description -> model |> addTodoAction model.generatedId description
  RemoveTodo todo -> model |> removeTodoAction todo
  UpdateNewTodoInput newValue -> model |> updateNewTodoInputAction newValue
  UpdateNewTodoId idString -> model { generatedId = idString }
  SetDone todo -> model { todos = model.todos |> updateTodo (todo { completed = true }) }

type TodoDispatch
  = TodoAction -> Effect Unit

newTodo :: String -> String -> Todo
newTodo generatedId description =
  { id: generatedId
  , description: description
  , completed: false
  }

addTodoAction :: String -> String -> Model -> Model
addTodoAction generatedId description model =
  model
    { todos = model.todos |> Array.cons (newTodo generatedId description)
    , newTodoInput = ""
    }

removeTodoAction :: Todo -> Model -> Model
removeTodoAction deletedTodo model =
  model
    { todos = model.todos |> Array.filter (\todo -> todo.id /= deletedTodo.id) }

updateNewTodoInputAction :: String -> Model -> Model
updateNewTodoInputAction newValue model = model { newTodoInput = newValue }

updateTodo :: Todo -> Array Todo -> Array Todo
updateTodo updatedTodo allTodos =
  let
    indices = case Array.findIndex (\todo -> todo.id == updatedTodo.id) allTodos of
      Just index -> [ index ]
      Nothing -> []
  in
    allTodos |> Array.modifyAtIndices indices (\_ -> updatedTodo)

-- VIEW
todoListRoot :: React.Component {}
todoListRoot = do
  todoReducer <- React.mkReducer update
  firstTodoId <- UUID.genUUID
  React.component "TodoComponent" \_ -> React.do
    model /\ dispatch <- React.useReducer (initialModel (show firstTodoId)) (todoReducer)
    React.useEffect model.todos do
      newTodoId <- UUID.genUUID
      dispatch $ UpdateNewTodoId $ show newTodoId
      pure mempty
    pure
      ( R.div
          { children:
              [ newTodoView dispatch model
              , viewTodos dispatch model
              , viewCompletedTodos dispatch model
              ]
          }
      )

newTodoView :: TodoDispatch -> Model -> React.JSX
newTodoView dispatch model =
  ( R.div
      { children:
          [ R.input
              { value: model.newTodoInput
              , placeholder: "New todo"
              , onChange:
                  Event.handler targetValue $ \value -> dispatch (UpdateNewTodoInput $ fromMaybe model.newTodoInput value)
              }
          , R.button { children: [ R.text "Add" ], onClick: Event.handler_ $ dispatch $ AddTodo model.newTodoInput }
          ]
      }
  )

viewTodos :: TodoDispatch -> Model -> React.JSX
viewTodos dispatch model =
  R.div
    { children:
        [ R.h2 { children: [ R.text "Current Tasks" ] }
        , R.ul
            { children:
                model.todos
                  |> Array.filter (\todo -> todo.completed == false)
                  |> map (\todo -> viewTodo dispatch todo model)
            }
        ]
    }

viewCompletedTodos :: TodoDispatch -> Model -> React.JSX
viewCompletedTodos dispatch model =
  R.div
    { children:
        [ R.h2 { children: [ R.text "Completed Tasks" ] }
        , R.ul
            { children:
                model.todos
                  |> Array.filter (\todo -> todo.completed)
                  |> map (\todo -> viewTodo dispatch todo model)
            }
        ]
    }

viewTodo :: TodoDispatch -> Todo -> Model -> React.JSX
viewTodo dispatch todo model =
  R.li
    { key: todo.id
    , children:
        [ (R.text todo.description)
        , if todo.completed then R.text " (Done)" else R.text ""
        , if not todo.completed then
            R.button
              { onClick: Event.handler_ (dispatch (SetDone todo))
              , children: [ R.text "Set Done" ]
              }
          else
            R.text ""
        , R.button
            { onClick: Event.handler_ (dispatch (RemoveTodo todo))
            , children: [ R.text "Delete" ]
            }
        ]
    }
