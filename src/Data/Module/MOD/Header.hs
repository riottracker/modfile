{-# LANGUAGE RecordWildCards #-}

module Data.Module.MOD.Header ( 
      Header (..)
    , getHeader
    , putHeader
    ) where

import Control.Applicative
import Control.Monad
import Data.Word
import Data.Module.MOD.Sample

data Header = Header { songName    :: [Word8] -- 20 bytes, space- or zero-padded
                     , sampleDescs :: [SampleDesc] -- traditionally 15 samples, but mostly 31.
                     , songLength  :: Word8 -- Song length in patterns (up to 128)
                     , restartByte :: Word8 -- Dunno about this
                     -- 128 bytes, each normally contains numbers between 0 and 63.
                     , orderList   :: [Word8] 
                     -- 4 letters _may_ be present, they indicate capabilities
                     --   of the tracker and have some implications: 
                     , idLetters   :: Maybe Word32 
                     }
    deriving (Show, Eq)

getHeader :: Get Header
getHeader = Header <$> replicateM 20 getWord8
                   -- TODO
                   -- you can't just progressively read, to know how
                   -- many sample info entries to read here one must know
                   -- whether the id letters are present. So first you need to
                   -- check offset 1080d for whether there's a valid letter 
                   -- combination there

putHeader :: Header -> Put
-- TODO
