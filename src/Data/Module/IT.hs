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

import Data.Module.IT.Header
import Data.Module.IT.Instrument
import Data.Module.IT.Sample
import Data.Module.IT.Pattern

data Module = Module { header         :: Header
                     , orders         :: [Word8]
                     , instruments    :: [Instrument]
                     , sampleHeaders  :: [SampleHeader]
                     , patterns       :: [Pattern]
                     }
    deriving (Show, Eq)

getModule :: Get Module
getModule = do
    header <- getHeader
    orders <- replicateM (fromIntegral (ordNum header))  getWord8
    instruments <- replicateM (fromIntegral (insNum header)) getInstrument
    sampleHeaders <- replicateM (fromIntegral (smpNum header)) getSampleHeader
    patterns <- replicateM (fromIntegral (patNum header)) getPattern
    return $ Module{..}

putModule :: Module -> Put
putModule Module{..} = do
    putHeader header
    mapM_ putWord8 orders
    mapM_ putInstrument instruments
    mapM_ putSampleHeader sampleHeaders
    mapM_ putPattern patterns

