{-# LANGUAGE TemplateHaskell #-}

module Node (
      Node ( Node )
    , bias
    , state
    , makeDefaultNode
    ) where

import qualified Data.Map.Lazy as M
import Control.Lens
import ActivationFunctions
import Internal
import Types

makeDefaultNode :: Node
makeDefaultNode = Node 0 0 (fn fnSigmoid) (deriv fnSigmoid) 0
