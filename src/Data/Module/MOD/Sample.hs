{-# LANGUAGE RecordWildCards #-}

module Data.Module.MOD.Sample (
    Sample (..)
    , SampleRec (..)
    , getSampleRec
    , getSample
    , putSample
    ) where

import Control.Applicative
import Control.Monad
import Data.Word
import Data.Binary.Get

data SampleRec = SampleRec { sampleName :: [Word8] -- 22 bytes, zero-padded
                             , halfSampleLength :: Word32 -- signed integer maybe?
                             , finetune :: Word8 -- finetune byte, only lower nibble used
                             , sampleVol :: Word8
                             , halfLoopStart :: Word16
                             , halfLoopLength :: Word16
                             }

type SampleData = [Word8]

{-data Sample = Sample { desc  ::  SampleRec-}
                     {-, sdata :: SampleData-}
                     {-}-}

getSampleRec :: Get SampleRec
getSampleRec = SampleRec <$> replicateM 22 getWord8
                         <*> getWord32le
                         <*> getWord8
                         <*> getWord8
                         <*> getWord16le
                         <*> getWord16le

getSampleData :: Int -> Get SampleData
getSampleData x = replicateM x getWord8
