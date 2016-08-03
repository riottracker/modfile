{-# LANGUAGE RecordWildCards #-}

module Data.Module.IT.Pattern (
      Pattern (..)
    , getPattern
    , getEmptyPattern
    , putPattern
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put

data Pattern = Pattern { length :: Word16
                       , rows   :: Word16
                       , ppad0  :: [Word8]           -- 4 bytes
-- TODO
                       }
    deriving (Show, Eq)

getPattern :: Get Pattern
getPattern = do
    length <- getWord16le
    rows <- getWord16le
    ppad0 <- replicateM 4 getWord8
    -- TODO
    return $ Pattern{..}

getEmptyPattern :: Get Pattern
getEmptyPattern = return $ Pattern 0 64 [0,0,0,0]

putPattern :: Pattern -> Put
putPattern Pattern{..} = do
    putWord16le length
    putWord16le rows
    mapM_ putWord8 ppad0

