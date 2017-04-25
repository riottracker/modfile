{-# LANGUAGE RecordWildCards #-}

module Codec.Tracker.IT (
      Module(..)
    , getModule
    , putModule
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Codec.Tracker.IT.Header
import           Codec.Tracker.IT.Instrument
import           Codec.Tracker.IT.Pattern
import           Codec.Tracker.IT.Sample

import           Util


data Module = Module { header        :: Header
                     , orders        :: [Word8]
                     , insOffsets    :: [Word32]
                     , smpOffsets    :: [Word32]
                     , patOffsets    :: [Word32]
                     , message       :: [Word8]
                     , instruments   :: [Instrument]
                     , sampleHeaders :: [SampleHeader]
                     , patterns      :: [Pattern]
                     }
    deriving (Show, Eq)


getModule :: Get Module
getModule = do
    header <- getHeader
    message <- getAtOffset (replicateM (fromIntegral $ messageLength header) getWord8) $ messageOffset header
    orders <- replicateM (fromIntegral (songLength header)) getWord8
    insOffsets <- replicateM (fromIntegral (numInstruments header)) getWord32le
    smpOffsets <- replicateM (fromIntegral (numSamples header)) getWord32le
    patOffsets <- replicateM (fromIntegral (numPatterns header)) getWord32le
    instruments <- mapM (getAtOffset getInstrument) insOffsets
    sampleHeaders <- mapM (getAtOffset getSampleHeader) smpOffsets
    patterns <- sequence [ if x == 0
                           then getEmptyPattern
                           else getAtOffset getPattern x
                         | x <- patOffsets ]
    return Module{..}

putModule :: Module -> Put
putModule Module{..} = do
    putHeader header
    mapM_ putWord8 orders
    mapM_ putWord32le insOffsets
    mapM_ putWord32le smpOffsets
    mapM_ putWord32le patOffsets
--    mapM_ putInstrument instruments
--    mapM_ putSampleHeader sampleHeaders
--    mapM_ putPattern patterns

