{-# LANGUAGE RecordWildCards #-}

module Codec.Tracker.XM.SampleHeader
    ( getSampleHeader
    , putSampleHeader
    , SampleHeader (..)
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Word


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
getSampleHeader = label "XM.Sample SampleHeader" $
                  SampleHeader <$> getWord32le <*> getWord32le <*> getWord32le
                               <*> getWord8 <*> getWord8 <*> getWord8
                               <*> getWord8 <*> getWord8 <*> getWord8
                               <*> replicateM 22 getWord8

putSampleHeader :: SampleHeader -> Put
putSampleHeader SampleHeader{..} = do
    mapM_ putWord32le [sampleLength, loopStart, loopLength]
    mapM_ putWord8 [volume, finetune, sampleType, panning, relativeNote, reserved]
    mapM_ putWord8 name

