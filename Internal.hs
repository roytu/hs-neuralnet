{-# LANGUAGE TemplateHaskell #-}

module Internal (
      Node ( Node )
    , bias
    , prevDeltaBias
    , activationFn
    , derivActivationFn
    , state
    , Link ( Link )
    , weight
    , prevDeltaWeight
    ) where

import Types
import Control.Lens

data Node = Node {
      _bias :: Bias
    , _prevDeltaBias :: Bias
    , _activationFn :: Double -> Double
    , _derivActivationFn :: Double -> Double
    , _state :: State
    }
makeLenses ''Node

instance Show Node where
    show = show . view state

data Link = Link {
      _weight :: Weight
    , _prevDeltaWeight :: Weight
    } deriving Show
makeLenses ''Link
