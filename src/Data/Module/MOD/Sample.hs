{-# LANGUAGE RecordWildCards #-}

module Data.Module.MOD.Sample (
    Sample (..)
    SampleDesc (..)
    , getSample
    , putSample
    ) where

data SampleDesc = SampleDesc { sampleName :: [Word8] -- 22 bytes, zero-padded
                             , halfSampleLength :: Word32 -- signed integer maybe?
                             , finetune :: Word8 -- finetune byte, only lower nibble used
                             , sampleVol :: Word8
                             , halfLoopStart :: Word16
                             , halfLoopLength :: Word16
                             }

type SampleData = [Word8]

data Sample = Sample { desc SampleDesc
                     , data SampleData
                     }


getSampleDesc :: Get SampleDesc
getSampleDesc = SampleDesc <$> replicateM 22 getWord8
                           <*> getWord32le
                           <*> getWord8
                           <*> getWord8
                           <*> getWord16le
                           <*> getWord16le

getSampleData :: SampleDesc -> Get SampleData
getSampleData d =  -- TODO: read length words of signed 8-bit integers
    where length = (halfSampleLength d) * 2 -- dunno
