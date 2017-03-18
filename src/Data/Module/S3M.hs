{-# LANGUAGE RecordWildCards #-}

module Data.Module.S3M (
      Module (..)
    , getModule
    , putModule
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Word

import           Data.Module.S3M.Header
import           Data.Module.S3M.Instrument
import           Data.Module.S3M.Pattern

import           Util

import Debug.Trace

data Module = Module { header      :: Header
                     , orders      :: [Word8]
                     , insOffsets  :: [Word16]
                     , patOffsets  :: [Word16]
                     , panning     :: [Word8]
                     , instruments :: [Instrument]
                     , patterns    :: [Pattern]
                     }
    deriving (Show, Eq)


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

putModule :: Module -> Put
putModule Module{..} = do
    putHeader header
    mapM_ putWord8 orders
    mapM_ putWord16le insOffsets
    mapM_ putWord16le patOffsets
    fail "todo: write instruments and patterns"

