module Util (
      getAtOffset
      ) where

import           Data.Binary.Get

getAtOffset :: Integral i => Get a -> i -> Get a
getAtOffset f n = lookAhead $ (>> f) . skip . (-) (fromIntegral n) . fromIntegral =<< bytesRead

