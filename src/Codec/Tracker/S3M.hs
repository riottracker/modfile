{-# LANGUAGE RecordWildCards #-}

-- | read/write Scream Tracker 3 files
module Codec.Tracker.S3M (
      Module (..)
    , getModule
    , putModule
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Data.Maybe

import           Codec.Tracker.S3M.Header
import           Codec.Tracker.S3M.Instrument
import           Codec.Tracker.S3M.Pattern

import           Util

-- | A Scream Tracker 3 module
data Module = Module { header      :: Header
                     , orders      :: [Word8]
                     , panning     :: [Word8]
                     , instruments :: [Instrument]
                     , patterns    :: [Pattern]
                     }
    deriving (Show, Eq)

-- | Read a `Module` from the monad state.
getModule :: Get Module
getModule = do
    header <- getHeader
    orders <- replicateM (fromIntegral (songLength header)) getWord8
    insOffsets <- replicateM (fromIntegral (numInstruments header)) getWord16le
    patOffsets <- replicateM (fromIntegral (numPatterns header)) getWord16le
    panning <- if defaultPanFlag header == 252 then replicateM 32 getWord8 else return []
    instruments <- mapM (getAtOffset getInstrument . (16*)) insOffsets
    patterns <- mapM (getAtOffset getPattern . (16*)) patOffsets
    return Module{..}

-- | Write a `Module` to the buffer.
putModule :: Module -> Put
putModule Module{..} = do
    putHeader header
    mapM_ putWord8 orders
    let body = 96 + length orders + 2 * (length instruments + length patterns) + length panning
    mapM_ (putWord16le . fromIntegral) [ body + i * 80 | i <- [0..length instruments - 1]]
    mapM_ (putWord16le . fromIntegral) $ scanl (\x y -> x + (fromIntegral $ packedLength y)) (body + 80 * length instruments) patterns
    mapM_ putWord8 panning
    mapM_ putInstrument instruments
    mapM_ putPattern patterns
  where insSize Instrument{..} = 13 + (if isJust pcmSample then 67 else 0) + (if isJust adlibSample then 67 else 0)
