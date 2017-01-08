{-# LANGUAGE RecordWildCards #-}

module Data.Module.S3M.Instrument (
      Instrument (..)
    , getInstrument
    , putInstrument
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Maybe
import           Data.Binary.Get
import           Data.Binary.Put

import           Data.Module.S3M.Instrument.PCM
import           Data.Module.S3M.Instrument.Adlib

data Instrument = Instrument { instrumentType :: Word8     -- 0: empty, 1: PCM, 2: adlib melody instrument, 3-7: adlib percussion instrument
                             , filename       :: [Word8]   -- 12 bytes 
                             , pcmSample      :: Maybe PCMSample
                             , adlibSample    :: Maybe AdlibSample
                             }
    deriving (Show, Eq)

getInstrument :: Get Instrument
getInstrument = do
     instrumentType <- getWord8
     filename <- replicateM 12 getWord8
     pcmSample <- sequence $ if instrumentType == 1 then Just getPCMSample else Nothing
     adlibSample <- sequence $ if instrumentType `elem` [2..7] then Just getAdlibSample else Nothing
     return Instrument{..}

putInstrument :: Instrument -> Put
putInstrument Instrument{..} = do
     putWord8 instrumentType
     mapM_ putWord8 filename
     when (isJust pcmSample) (putPCMSample $ fromJust pcmSample)
     when (isJust adlibSample) (putAdlibSample $ fromJust adlibSample)

