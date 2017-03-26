module Util (
        getAtOffset
      , getByMask
      , getToLimit
      ) where

import           Data.Binary.Get
import           Data.Bits


getAtOffset :: Integral i => Get a -> i -> Get a
getAtOffset f n = lookAhead $ (>> f) . skip . (-) (fromIntegral n) . fromIntegral =<< bytesRead

getByMask :: (Bits a, Monad m) => a -> Int -> m b -> m (Maybe b)
getByMask mask p f = sequence $ if testBit mask p then Just f else Nothing

getToLimit :: Integral i => Get a -> i -> Get [a]
getToLimit f limit = (flip toLimit) [] =<< bytesRead 
  where toLimit br0 lst = bytesRead >>=
            \br1 -> if (br1 - br0) < (fromIntegral limit) then
                        (toLimit br0) . (lst ++) . pure =<< f
                    else pure lst

