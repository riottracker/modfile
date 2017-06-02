{-# LANGUAGE RecordWildCards #-}

-- | read/write ImpulseTracker files
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


-- | An ImpulseTracker module
data Module = Module { header        :: Header
                     , orders        :: [Word8]
                     , message       :: [Word8]
                     , instruments   :: [Instrument]
                     , sampleHeaders :: [SampleHeader]
                     , patterns      :: [Pattern]
                     }
    deriving (Show, Eq)


-- | Read a `Module` from the monad state.
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

-- | Write a `Module` to the buffer.
putModule :: Module -> Put
putModule Module{..} = do
    putHeader header
    mapM_ putWord8 orders
    let body = 192 + length orders + 4 * (length instruments + length sampleHeaders + length patterns)
        ins  = 550 * (length instruments - 1)
        samp = 80 * (length sampleHeaders - 1)
    mapM_ (putWord32le . fromIntegral) [ length message + body + i * 550 | i <- [0..length instruments - 1]]
    mapM_ (putWord32le . fromIntegral) [ length message + body + ins + s * 80 | s <- [0..length sampleHeaders - 1]]
    mapM_ (putWord32le . fromIntegral) $ scanl (\x y -> x + (fromIntegral $ patternLength y)) 0 patterns
    mapM_ putWord8 message
    mapM_ putInstrument instruments
    mapM_ putSampleHeader sampleHeaders
    mapM_ putPattern patterns

