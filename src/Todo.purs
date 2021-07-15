module Todo (todoListRoot) where

import CoolPrelude
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.UUID as UUID
import Effect (Effect)
import Effect.Console (logShow)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events as Event
import React.Basic.Hooks ((/\))
import React.Basic.Hooks as React

type Todo
  = { id :: String
    , name :: String
    , completed :: Boolean
    , editField :: Maybe String
    }

data TodoAction
  = AddTodo String
  | RemoveTodo String
  | EditTodo Todo
  | UpdateNewTodoInput String
  | EnterEditMode Todo
  | UpdateNewTodoId String
  | SetDone Todo

type TodoModel
  = { name :: String
    , todos :: Array Todo
    , newTodoInput :: String
    , newTodoId :: String
    , generatedId :: String
    }

type TodoDispatch
  = TodoAction -> Effect Unit

initialModel :: String -> TodoModel
initialModel generatedId =
  { name: "Unnamed"
  , todos: []
  , newTodoInput: ""
  , newTodoId: ""
  , generatedId: generatedId
  }

newTodo :: String -> String -> Todo
newTodo generatedId description = { id: generatedId, name: description, completed: false, editField: Nothing }

addTodoAction :: String -> String -> TodoModel -> TodoModel
addTodoAction generatedId description model =
  model
    { todos =
      model.todos
        |> Array.cons (newTodo generatedId description)
    , newTodoInput = ""
    }

removeTodoAction :: String -> TodoModel -> TodoModel
removeTodoAction todoId model =
  model
    { todos =
      model.todos
        |> Array.filter (\todo -> todo.id /= todoId)
    }

editTodoAction :: Todo -> TodoModel -> TodoModel
editTodoAction newTodo model =
  let
    todoIndex = Array.findIndex (\todo -> todo.id == newTodo.id) model.todos
  in
    maybe model (\index -> model { todos = model.todos }) todoIndex

enterEditModeAction :: Todo -> TodoModel -> TodoModel
enterEditModeAction todo model =
  let
    todoIndex = Array.findIndex (\t -> t.id == todo.id) model.todos
  in
    maybe model (\index -> model { todos = model.todos |> updateTodo (todo { editField = Just todo.name }) }) todoIndex

updateNewTodoInputAction :: String -> TodoModel -> TodoModel
updateNewTodoInputAction newValue model = model { newTodoInput = newValue }

updateTodo :: Todo -> Array Todo -> Array Todo
updateTodo updatedTodo allTodos =
  let
    indices = case Array.findIndex (\todo -> todo.id == updatedTodo.id) allTodos of
      Just index -> [ index ]
      Nothing -> []
  in
    allTodos |> Array.modifyAtIndices indices (\_ -> updatedTodo)

updateTodoModel :: TodoModel -> TodoAction -> TodoModel
updateTodoModel model action = case action of
  AddTodo description -> model |> addTodoAction model.generatedId description
  RemoveTodo todoId -> model |> removeTodoAction todoId
  EditTodo updatedTodo -> model |> editTodoAction updatedTodo
  UpdateNewTodoInput newValue -> model |> updateNewTodoInputAction newValue
  EnterEditMode todo -> model |> enterEditModeAction todo
  UpdateNewTodoId idString -> model { generatedId = idString }
  SetDone todo -> model { todos = model.todos |> updateTodo (todo { completed = true }) }

todoListRoot :: React.Component {}
todoListRoot = do
  todoReducer <- React.mkReducer updateTodoModel
  firstTodoId <- UUID.genUUID
  React.component "TodoComponent" \_ -> React.do
    model /\ dispatch <- React.useReducer (initialModel (show firstTodoId)) (todoReducer)
    React.useEffect model.todos do
      logShow "Hello"
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

newTodoView :: TodoDispatch -> TodoModel -> React.JSX
newTodoView dispatch model =
  ( R.div
      { children:
          [ R.input
              { value: model.newTodoInput
              , placeholder: "New todo"
              , onChange:
                  React.do
                    Event.handler targetValue
                      ( \value ->
                          dispatch (UpdateNewTodoInput $ fromMaybe model.newTodoInput value)
                      )
              }
          , R.button { children: [ R.text "Add" ], onClick: Event.handler_ $ dispatch $ AddTodo model.newTodoInput }
          ]
      }
  )

viewTodos :: TodoDispatch -> TodoModel -> React.JSX
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

viewCompletedTodos :: TodoDispatch -> TodoModel -> React.JSX
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

viewTodo :: TodoDispatch -> Todo -> TodoModel -> React.JSX
viewTodo dispatch todo model =
  R.li
    { key: todo.id
    , children:
        [ maybe (R.text todo.name) (\_ -> R.text "Edit mode") todo.editField
        , if todo.completed then R.text " (Done)" else R.text ""
        , if not todo.completed then
            R.button
              { onClick: Event.handler_ (dispatch (SetDone todo))
              , children: [ R.text "Set Done" ]
              }
          else
            R.text ""
        , R.button
            { onClick: Event.handler_ (dispatch (RemoveTodo todo.id))
            , children: [ R.text "Delete" ]
            }
        ]
    }
