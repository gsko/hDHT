module DHT.TokenBucket where 

import Data.Word
import Data.Time.Clock

data TokenBucket = TokenBucket {
    rate :: Word64
  , tokens :: Word64
  , maxTokens :: Word64
}
