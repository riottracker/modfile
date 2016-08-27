{-# LANGUAGE RecordWildCards #-}

module Data.Module.MOD.Header ( 
      Header (..)
    , getHeader
    , putHeader
    ) where

import Control.Applicative
import Control.Monad
import Data.Word

data SampleDesc = SampleDesc { sampleName   :: [Word8] -- 22 bytes, zeroe-padded
                             , halfSampleLength :: Word32  -- 
                             , finetune :: Word8 -- finetune byte, only the lower nibble is considered
                             , sampleVol :: Word8
                             , halfLoopStart :: Word16
                             , halfLoopLength :: Word16
                             }

data Header = Header { songName   :: [Word8] -- 20 bytes, space- or zero-padded
                     , sampleInfo :: [SampleDesc] -- traditionally 15 samples, but mostly 31.
                     , songLength :: Word8 -- Song length in patterns (up to 128)
                     , restartByte :: Word8 -- Dunno about this
                     -- 128 bytes, each normally contains numbers between 0 and 63.
                     , orderList   :: [Word8] 
                     -- 4 letters _may_ be present, they indicate capabilities
                     --   of the tracker and have some implications: 
                     , idLetters   :: Maybe Word32 
                     }
    deriving (Show, Eq)

-- This is some serious Kohlrabi Magic here, stolen from IT.hs. Dunno if I can
-- use it as is.
getAtOffset :: Get a -> Word32 -> Get a
getAtOffset f n = (>> f) . skip . (-) (fromIntegral n) . fromIntegral =<< bytesRead

getSampleDesc :: Get SampleDesc
getSampleDesc = SampleDesc <$> geplicateM 22 getWord8
                           <*> getWord32le
                           <*> getWord8
                           <*> getWord8
                           <*> getWord16le
                           <*> getWord16le

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
