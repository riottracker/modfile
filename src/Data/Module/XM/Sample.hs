{-# LANGUAGE RecordWildCards #-}

module Data.Module.XM.Sample (
      Sample (..)
    , getSample
    , putSample
    , SampleHeader (..)
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put

data SampleHeader = SampleHeader { sampleLength   :: Word32
                                 , loopStart      :: Word32
                                 , loopLength     :: Word32
                                 , volume         :: Word8
                                 , finetune       :: Word8
                                 , sampleType     :: Word8
                                 , panning        :: Word8
                                 , relativeNote   :: Word8
                                 , reserved       :: Word8
                                 , name           :: [Word8]    -- 22 bytes
                                 }
                             deriving (Show, Eq)

data Sample = Sample { sampleHeader :: SampleHeader
                     , sampleData   :: [Word8]
                     }
                 deriving (Show, Eq)

getSampleHeader :: Get SampleHeader
getSampleHeader = SampleHeader <$> getWord32le <*> getWord32le <*> getWord32le
                               <*> getWord8 <*> getWord8 <*> getWord8
                               <*> getWord8 <*> getWord8 <*> getWord8
                               <*> replicateM 22 getWord8

putSampleHeader :: SampleHeader -> Put
putSampleHeader SampleHeader{..} = do
    mapM_ putWord32le [sampleLength, loopStart, loopLength]
    mapM_ putWord8 [volume, finetune, sampleType, panning, relativeNote, reserved]
    mapM_ putWord8 name

getSample :: Get Sample
getSample = do
    sampleHeader <- getSampleHeader
    sampleData <- replicateM (fromIntegral (sampleLength sampleHeader)) getWord8
    return $ Sample{..}

putSample :: Sample -> Put
putSample Sample{..} = do
    putSampleHeader sampleHeader
    mapM_ putWord8 sampleData

