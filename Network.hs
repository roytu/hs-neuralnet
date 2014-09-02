{-# LANGUAGE TemplateHaskell #-}

module Network (
    ) where

import NodeRef
import Control.Lens
import Control.Monad.Writer.Lazy
import Control.Applicative
import Data.Set

data Network = Network {
      _inputNodes :: [NodeRef]
    , _outputNodes :: [NodeRef]
    } deriving Show
makeLenses ''Network

type Layer = [NodeRef]

linkNodeToLayer :: NodeRef -> Layer -> (NodeRef, Layer)
linkNodeToLayer i olayer = foldr (\o (i, os) -> (linkNodes i o 0) & _2 %~ (:os)) (i, []) olayer -- wow

linkLayers :: Layer -> Layer -> (Layer, Layer)
linkLayers ilayer olayer = foldr (\i (is, ol) -> (linkNodeToLayer i ol) & _1 %~ (:is)) ([], olayer) ilayer -- so beautiful

emptyNetwork :: Network
emptyNetwork = Network [] []

makeNode :: Int -> Writer Layer Int
makeNode n = do
    let nodeRef = makeDefaultNodeRef n
    tell [nodeRef]
    return (succ n)

makeLayer :: Int -> Int -> Writer Layer Int
makeLayer nodeCount n = foldl1 (>=>) (replicate nodeCount makeNode) $ n

makeLayers :: [Int] -> [Layer]
makeLayers layerCounts = execWriter $ foldl1 (>=>) (map (listify `compose2` makeLayer) layerCounts) $ 0
    where
        listify :: Writer a b -> Writer [a] b
        listify = mapWriter (_2 %~ pure)

        compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
        compose2 = (.) . (.)

linkAndWrite :: Layer -> Layer -> Writer [Layer] Layer
linkAndWrite i o = do
    let (i', o') = linkLayers i o
    tell [i']
    return o'

-- please kill me
makeDefaultNetwork :: [Int] -> Network
makeDefaultNetwork layerCounts = Network <$> head <*> last $ layers'
    where
        layers' :: [Layer]
        layers' = (\(a, b) -> b ++ [a]) $ runWriter $ foldl1 (>=>) (map (flip linkAndWrite) $ tail layers) $ head layers

        layers :: [Layer]
        layers = makeLayers layerCounts

type Pattern = (
      [State] -- Initial states
    , [Expectation] -- Expectation value
    , [Expectation -> Bool] -- Conditions
    , [String] -- Failure strings
    )
type LearnRate = Double
type MomentumRate = Double
type StopEarly = Boolean

-- Find unique outputs of Nodes
findOutputs :: Layer -> Layer
findOutputs ilayer = toList . fromList $ map (view outputs) ilayer

-- Update state of all neurons by fire
fireAll :: Network -> Network
fireAll network = network & inputs



    where
        fireLayer :: Layer -> Layer
        fireLayer layer = map fire layer

-- Train network once on one pattern
trainSingle :: Pattern -> LearnRate -> MomentumRate -> StopEarly -> Network -> Network
trainSingle (inputs, expectations, _, _) learnRate momentumRate stopEarly network = 
    where
        primedNetwork = network & (inputNodes %~ zipWith setState inputs)
                                . (outputNodes %~ zipWith setExpectation expectations)

train :: [Pattern] -> LearnRate -> MomentumRate -> StopEarly -> Network -> Network
train patterns learnRate momentumRate stopEarly network = do
    let nextNetwork network = 
        
{-|
    def train(self, patterns, learnRate=0.9, momentumRate=0.4, stopEarly=False):
        for (inputStates, exps, _, _) in patterns:
            for (s, i) in zip(inputStates, self.inputNodes):
                i.setState(s)
            for (e, o) in zip(exps, self.outputNodes):
                o.setExpectation(e)

            self.fireAll()
            self.backpropagate(learnRate, momentumRate)


        # Stop when all patterns succeed
        if stopEarly:
            allPatternsTrained = True
            for (inputStates, _, conds, _) in patterns:
                for (s, i) in zip(inputStates, self.inputNodes):
                    i.setState(s)
                self.fireAll()
                for (out, cond) in zip(map(lambda o: o.state, self.outputNodes), conds):
                    if not cond(out):
                        allPatternsTrained = False
                        break
            if allPatternsTrained:
                return True
        return False
|-}




{-|
Here's some (maybe) better code from Zekka:

foldr (\o (i, os) -> (linkNodes i o 0) & _2 %~ flip (:) os) (i, []) olayer

First things first, let's rewrite the expression with (:) to use a section.

foldr (\o (i, os) -> (linkNodes i o 0) & _2 %~ (:os)) (i, []) olayer

Using a more idiomatic formulation of linkNodes we get this:

foldr (\o (i, os) -> (linkNodes' 0 i o) & _2 %~ (:os)) (i, []) olayer

What each iteration ultimately depends on is a NodeRef from the olayer, a starting NodeRef (based on the result of the last iteration) and a list of NodeRefs (to build on).

Hence the sig of the body should look something like this:

body :: NodeRef -> NodeRef -> Writer (Dual [NodeRef]) NodeRef

If we can get that, then the main body looks like this:

f olayer i = runWriter (foldr (>=>) (map body olayer)) i 

Or well:

f olayer = runWriter (foldr (>=>) (map body olayer)) 

body :: NodeRef -> NodeRef -> Writer (Dual [NodeRef]) NodeRef
body o i = do
    (n, l) <- linkNodes' 0 i o
    tell l
    return n

f olayer = runWriter (foldr (>=>) (map (do { (n, l) <- linkNodes' 0 i o; tell l; return n }) olayer))

|-}
