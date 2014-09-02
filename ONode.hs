module ONode (
      getState
    ) where

import Node
import Types

getState :: Node -> State
getState node = node^.state

setExpectation :: Expectation -> Node -> Node
setExpectation e node = node & e ~. e
