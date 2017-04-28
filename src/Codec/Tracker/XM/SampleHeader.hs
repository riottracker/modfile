{-# LANGUAGE RecordWildCards #-}

-- | read/write Fasttracker II samples
module Codec.Tracker.XM.SampleHeader
    ( getSampleHeader
    , putSampleHeader
    , SampleHeader (..)
    , Sample (..)
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

-- | Fasttracker II sample header
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

-- | Fasttracker II sample
data Sample = Sample { sampleHeader :: SampleHeader
                     , sampleData   :: [Word8]
                     }
                 deriving (Show, Eq)

-- | Read a `SampleHeader` from the monad state.
getSampleHeader :: Get SampleHeader
getSampleHeader = label "XM.Sample SampleHeader" $
                  SampleHeader <$> getWord32le <*> getWord32le <*> getWord32le
                               <*> getWord8 <*> getWord8 <*> getWord8
                               <*> getWord8 <*> getWord8 <*> getWord8
                               <*> replicateM 22 getWord8

-- | Write a `SampleHeader` to the buffer.
putSampleHeader :: SampleHeader -> Put
putSampleHeader SampleHeader{..} = do
    mapM_ putWord32le [sampleLength, loopStart, loopLength]
    mapM_ putWord8 [volume, finetune, sampleType, panning, relativeNote, reserved]
    mapM_ putWord8 name

