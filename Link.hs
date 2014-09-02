module Link (
      Link ( Link )
    , weight
    , prevDeltaWeight
    , makeLink
    ) where

import Internal
import Types

makeLink :: Weight -> Link
makeLink weight = Link weight 0
