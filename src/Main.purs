module Main where

import CoolPrelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (throw)
import React.Basic.DOM (render)
import Todo (todoListRoot)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.HTML (window)
import Web.HTML.HTMLDocument (body)
import Web.HTML.HTMLElement (toParentNode)
import Web.HTML.Window (document)

main :: Effect Unit
main = do
  body <- body =<< document =<< window
  case body of
    Nothing -> throw "Could not find body."
    Just b -> do
      rootNode <- querySelector (QuerySelector "#root") (toParentNode b)
      case rootNode of
        Nothing -> throw "Could not find root node"
        Just node -> do
          app <- todoListRoot
          render (app {}) (node)
