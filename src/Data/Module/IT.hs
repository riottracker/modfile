{-# LANGUAGE RecordWildCards #-}

module Data.Module.IT (
      Module (..)
    , getModule
    , putModule
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put

import           Numeric (showHex)

import Data.Module.IT.Header
import Data.Module.IT.Instrument
import Data.Module.IT.Sample
import Data.Module.IT.Pattern

data Module = Module { header         :: Header
                     , orders         :: [Word8]
                     , insOffsets     :: [Word32]
                     , smpOffsets     :: [Word32]
                     , patOffsets     :: [Word32]
--                     , instruments    :: [Instrument]
--                     , sampleHeaders  :: [SampleHeader]
--                     , patterns       :: [Pattern]
                     }
    deriving (Show, Eq)

getModule :: Get Module
getModule = do
    header <- getHeader
    orders <- replicateM (fromIntegral (ordNum header))  getWord8
    insOffsets <- replicateM (fromIntegral (insNum header)) getWord32le
    smpOffsets <- replicateM (fromIntegral (smpNum header)) getWord32le
    patOffsets <- replicateM (fromIntegral (patNum header)) getWord32le
--    instruments <- replicateM (fromIntegral (insNum header)) getInstrument
--    sampleHeaders <- replicateM (fromIntegral (smpNum header)) getSampleHeader
--    patterns <- replicateM (fromIntegral (patNum header)) getPattern
    return $ Module{..}

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

