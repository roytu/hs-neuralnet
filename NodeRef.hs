{-# LANGUAGE TemplateHaskell #-}

module NodeRef (
      NodeRef
    , makeDefaultNodeRef
    , linkNodes
-- Temporary
    , inputs
    , outputs
    , links
    ) where

import Node
import Link
import Types
import Control.Applicative
import Data.Hashable
import Data.Maybe
import qualified Data.HashMap.Lazy as M
import Control.Lens

data NodeRef = NodeRef {
      _key :: Int
    , _node :: Node
    , _inputs :: [NodeRef]
    , _outputs :: [NodeRef]
    , _links :: M.HashMap NodeRef Link
    }
makeLenses ''NodeRef

makeDefaultNodeRef :: Int -> NodeRef
makeDefaultNodeRef key = NodeRef key makeDefaultNode [] [] M.empty 

-- | Link input to output and return (input, output)
linkNodes :: NodeRef -> NodeRef -> Weight -> (NodeRef, NodeRef)
linkNodes inode onode weight = (inode', onode')
    where
        inode' = inode & (outputs %~ (:) onode)
                       . (links %~ M.insert onode (makeLink weight))
        onode' = onode & (inputs %~ (:) inode)
                       . (links %~ M.insert inode (makeLink weight))

-- | Update state of node. DOES NOT PROPAGATE
fire :: NodeRef -> NodeRef
fire nodeRef
    | length (nodeRef ^. inputs) == 0 = error "Attempt to fire input node"
    | otherwise                       = nodeRef & state .~ state'
        where
            state' = nodeRef ^. node ^. activationFn $ potential
            potential = (sum $ (*) <$> getState <*> getWeight <$> nodeRef ^. inputs) + getBias
                where
                    getBias = nodeRef ^. node ^. bias
                    getWeight = view weight . fromJust . flip M.lookup (nodeRef ^. links)
                    getState = view state . view node

instance Show NodeRef where
    show = show . view key

instance Eq NodeRef where
    a == b = a ^. key == b ^. key
    a /= b = a /= b

instance Ord NodeRef where
    compare = compare . view key
    (<)     = (<) . view key
    (>=)    = (>=) . view key
    (>)     = (>) . view key
    (<=)    = (<=) . view key
    max a b
        | max (a ^. key) (b ^. key) == a ^. key = a
        | otherwise                             = b
    min a b
        | min (a ^. key) (b ^. key) == a ^. key = a
        | otherwise                             = b

instance Hashable NodeRef where
    hashWithSalt salt = hash . (+) salt . view key
    hash = hash . view key 
