{-# LANGUAGE RecordWildCards #-}

module Data.Module.S3M (
      Module (..)
    , getModule
    , putModule
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put

import           Data.Module.S3M.Header
import           Data.Module.S3M.Instrument
import           Data.Module.S3M.Pattern

data Module = Module { header      :: Header
                     , orders      :: [Word8]
                     , insOffsets  :: [Word16]
                     , patOffsets  :: [Word16]
-- OPTIONAL:         , panning     :: [Word8]
                     , instruments :: [Instrument]
                     , patterns    :: [Pattern]
                     }
    deriving (Show, Eq)

getAtOffset :: Get a -> Word16 -> Get a
getAtOffset f n = lookAhead $ (>> f) . skip . (-) (fromIntegral n) . fromIntegral =<< bytesRead

getModule :: Get Module
getModule = do
    header <- getHeader
    orders <- replicateM (fromIntegral (numOrders header)) getWord8
    insOffsets <- replicateM (fromIntegral (numInstruments header)) getWord16le
    patOffsets <- replicateM (fromIntegral (numPatterns header)) getWord16le
    instruments <- mapM (getAtOffset getInstrument) insOffsets
    patterns <- mapM (getAtOffset getPattern) patOffsets
    return $ Module{..}

putModule :: Module -> Put
putModule Module{..} = do
    putHeader header
    mapM_ putWord8 orders
    mapM_ putWord16le insOffsets
    mapM_ putWord16le patOffsets
    fail "todo: write instruments and patterns"

