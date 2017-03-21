module Util (
        getAtOffset
      , getByMask
      ) where

import           Control.Monad
import           Data.Binary.Get
import           Data.Bits


getAtOffset :: Integral i => Get a -> i -> Get a
getAtOffset f n = lookAhead $ (>> f) . skip . (-) (fromIntegral n) . fromIntegral =<< bytesRead

getByMask :: (Bits a, Monad m) => a -> Int -> m b -> m (Maybe b)
getByMask mask p f = sequence $ if testBit mask p then Just f else Nothing

