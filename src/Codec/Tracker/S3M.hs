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
import           Codec.Tracker.S3M.Instrument.PCM
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
getModule = label "S3M" $ do
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
    putHeader header -- 96 bytes
    mapM_ putWord8 orders -- length orders bytes
    mapM_ (putWord16le . fromIntegral) [ pBody + i * 5 | i <- [0..length instruments - 1]]
    mapM_ (putWord16le . fromIntegral) $ init pPtrs
    mapM_ putWord8 panning
    mapM_ putWord8 $ replicate (16 - (body `rem` 16)) 0
    mapM_ (uncurry putInstrument) $ zip (scanl (+) (1 + last pPtrs) ((\x -> 1 + x `div` 16) . samSize <$> instruments)) instruments
    mapM_ (\p -> putPattern p >> (mapM_ putWord8 $ replicate (16 - ((packedSize p) `rem` 16)) 0)) patterns
    mapM_ (\i -> 
     maybe (return ()) ((\sd -> mapM_ putWord8 $ sd ++ (replicate (16 - ((length sd) `rem` 16)) 0)) . sampleData) (pcmSample i)
     ) instruments
  where samSize Instrument{..} = maybe 0 (length . sampleData) pcmSample
        body = 96 + length orders + 2 * (length instruments + length patterns) + length panning
        pBody = body `div` 16 + if body `rem` 16 > 0 then 1 else 0
        pPtrs = scanl (\x y -> x + (packedSize y) `div` 16 + (if (packedSize y) `rem` 16 > 0 then 1 else 0)) (pBody + 5 * length instruments) patterns

