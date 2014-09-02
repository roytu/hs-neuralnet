module INode (
      setState
    ) where

import Node

setState :: State -> Node -> Node
setState s node = node & state .~ s
