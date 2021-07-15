module Todo (todoListRoot) where

import CoolPrelude
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Effect (Effect)
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
  = AddTodo Todo
  | RemoveTodo String
  | EditTodo Todo
  | UpdateNewTodoInput String
  | EnterEditMode Todo

type TodoModel
  = { name :: String
    , todos :: Array Todo
    , newTodoInput :: String
    }

type TodoDispatch
  = TodoAction -> Effect Unit

initialModel :: TodoModel
initialModel =
  { name: "Unnamed"
  , todos:
      [ { name: "Test", id: "testId1", completed: false, editField: Nothing }
      , { name: "Test 2", id: "testId2", completed: false, editField: Nothing }
      ]
  , newTodoInput: ""
  }

addTodoAction :: Todo -> TodoModel -> TodoModel
addTodoAction newTodo model =
  model
    { todos =
      model.todos
        |> Array.cons newTodo
    , newTodoInput = ""
    }

removeTodoAction :: String -> TodoModel -> TodoModel
removeTodoAction todoId model =
  model
    { todos =
      model.todos
        |> Array.dropWhile (\todo -> todo.id == todoId)
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

updateTodoModel :: TodoModel -> TodoAction -> TodoModel
updateTodoModel model action = case action of
  AddTodo newTodo -> model |> addTodoAction newTodo
  RemoveTodo todoId -> model |> removeTodoAction todoId
  UpdateNewTodoInput newValue -> model |> updateNewTodoInputAction newValue
  EnterEditMode todo -> model |> enterEditModeAction todo

todoListRoot :: React.Component {}
todoListRoot = do
  todoReducer <- React.mkReducer updateTodoModel
  React.component "TodoComponent" \_ -> React.do
    model /\ dispatch <- React.useReducer initialModel (todoReducer)
    pure
      ( R.div
          { children: [ newTodoView dispatch model, viewTodos dispatch model ]
          }
      )

newTodoView :: TodoDispatch -> TodoModel -> React.JSX
newTodoView dispatch model =
  R.div
    { children:
        [ R.input
            { value: model.newTodoInput
            , placeholder: "New todo"
            , onChange:
                Event.handler targetValue
                  ( \value ->
                      dispatch (UpdateNewTodoInput $ fromMaybe model.newTodoInput value)
                  )
            }
        ]
    }

viewTodos :: TodoDispatch -> TodoModel -> React.JSX
viewTodos dispatch model =
  R.ul
    { children: map (\todo -> viewTodo dispatch todo model) model.todos }

viewTodo :: TodoDispatch -> Todo -> TodoModel -> React.JSX
viewTodo dispatch todo model =
  R.li
    { key: todo.id
    , children:
        [ maybe (R.text todo.name) (\_ -> R.text "Edit mode") todo.editField
        , R.button
            { onClick: Event.handler_ (dispatch (EnterEditMode todo))
            , children: [ R.text "Edit" ]
            }
        , R.button
            { onClick: Event.handler_ (dispatch (RemoveTodo todo.id))
            , children: [ R.text "Delete" ]
            }
        ]
    }
