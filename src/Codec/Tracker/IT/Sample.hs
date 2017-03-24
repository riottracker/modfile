{-# LANGUAGE RecordWildCards #-}

module Codec.Tracker.IT.Sample (
      SampleHeader (..)
    , getSampleHeader
    , putSampleHeader
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

data SampleHeader = SampleHeader { magicNumber   :: Word32    -- "IMPS"
                                 , fileName      :: [Word8]   -- 12 bytes
                                 , spad0         :: Word8
                                 , globalVolume  :: Word8
                                 , flags         :: Word8
                                 , defaultVolume :: Word8
                                 , name          :: [Word8]   -- 26 bytes
                                 , convert       :: Word16
                                 , sampleLength  :: Word32
                                 , loopBegin     :: Word32
                                 , loopEnd       :: Word32
                                 , c5Speed       :: Word32
                                 , susLoopBegin  :: Word32
                                 , susLoopEnd    :: Word32
                                 , samplePointer :: Word32
                                 , vibratoSpeed  :: Word8
                                 , vibratoDepth  :: Word8
                                 , vibratoRate   :: Word8
                                 , vibratoType   :: Word8
-- cache file:                   , fileSize      :: Word32
--                               , date          :: Word16
--                               , time          :: Word16
--                               , format        :: Word8
--
                       }
    deriving (Show, Eq)

getSampleHeader :: Get SampleHeader
getSampleHeader = label "IT.Sample SampleHeader" $
                  SampleHeader <$> getWord32le <*> replicateM 12 getWord8
                               <*> getWord8 <*> getWord8 <*> getWord8
                               <*> getWord8 <*> replicateM 26 getWord8
                               <*> getWord16le <*> getWord32le <*> getWord32le
                               <*> getWord32le <*> getWord32le <*> getWord32le
                               <*> getWord32le <*> getWord32le <*> getWord8
                               <*> getWord8 <*> getWord8 <*> getWord8

putSampleHeader :: SampleHeader -> Put
putSampleHeader SampleHeader{..} = do
    putWord32le magicNumber
    mapM_ putWord8 (fileName ++ [spad0, globalVolume, flags, defaultVolume] ++ name)
    putWord16le convert
    mapM_ putWord32le [ sampleLength, loopBegin, loopEnd, c5Speed, susLoopBegin, susLoopEnd, samplePointer ]
    mapM_ putWord8    [ vibratoSpeed, vibratoDepth, vibratoRate, vibratoType ]

