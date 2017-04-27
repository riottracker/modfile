-- | Various helper functions used to read/write module files.
module Util (
        getAtOffset
      , getByMask
      , getToLimit
      ) where

import           Data.Binary.Get
import           Data.Bits

-- | Run the given decoder at an offset (counting from the start) without consuming its input.
getAtOffset :: Integral i => Get a -> i -> Get a
getAtOffset f n = lookAhead $ (>> f) . skip . (-) (fromIntegral n) . fromIntegral =<< bytesRead

-- | Run the given decoder if the specified bit is set.
getByMask :: (Bits a, Monad m) => a -> Int -> m b -> m (Maybe b)
getByMask mask p f = sequence $ if testBit mask p then Just f else Nothing

-- | Repeatedly run a decoder until a certain offset is reached and collect the results.
getToLimit :: Integral i => Get a -> i -> Get [a]
getToLimit f limit = flip toLimit [] =<< bytesRead 
  where toLimit br0 lst = bytesRead >>=
            \br1 -> if br1 - br0 < fromIntegral limit then
                        toLimit br0 . (lst ++) . pure =<< f
                    else pure lst

