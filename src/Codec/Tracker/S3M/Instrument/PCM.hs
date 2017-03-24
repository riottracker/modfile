{-# LANGUAGE RecordWildCards #-}

module Codec.Tracker.S3M.Instrument.PCM (
      PCMSample(..)
    , getPCMSample
    , putPCMSample
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits


data PCMSample = PCMSample { ptrDataH     :: Word8
                           , ptrDataL     :: Word16
                           , sampleLength :: Word32
                           , loopStart    :: Word32
                           , loopEnd      :: Word32
                           , volume       :: Word8
                           , ppad0        :: Word8
                           , packed       :: Word8
                           , flags        :: Word8
                           , c2spd        :: Word32
                           , internal     :: [Word8]  -- 12 bytes
                           , title        :: [Word8]  -- 28 bytes
                           , sig          :: [Word8]  -- 4 bytes "SCRS" 
                           , sampleData   :: [Word8]
                           }
    deriving (Show, Eq)

getPCMSample :: Get PCMSample
getPCMSample = label "S3M.Instrument.PCM" $ do
     ptrDataH <- getWord8
     ptrDataL <- getWord16le
     sampleLength <- getWord32le
     loopStart <- getWord32le
     loopEnd <- getWord32le
     volume <- getWord8
     ppad0 <- getWord8
     packed <- getWord8
     flags <- getWord8
     c2spd <- getWord32le
     internal <- replicateM 12 getWord8
     title <- replicateM 28 getWord8
     sig <- replicateM 4 getWord8
     let offset = fromIntegral ptrDataL + (fromIntegral ptrDataH `shiftL` 16)
     br <- bytesRead
     sampleData <- lookAhead $ skip (offset - fromIntegral br) >> replicateM (fromIntegral sampleLength) getWord8
     return PCMSample{..}

putPCMSample :: PCMSample -> Put
putPCMSample PCMSample{..} = do
     putWord8 ptrDataH
     putWord16le ptrDataL
     mapM_ putWord32le [sampleLength, loopStart, loopEnd]
     mapM_ putWord8 [volume, ppad0, packed, flags]
     putWord32le c2spd
     mapM_ putWord8 (internal ++ title ++ sig)
     fail "writing pcm sample data is not implemented (yet)"
-- TODO


