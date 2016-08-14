{-# LANGUAGE RecordWildCards #-}

module Data.Module.IT.Header (
      Header (..)
    , getHeader
    , putHeader
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put
 
data Header = Header { magicNumber    :: Word32     -- "IMPM" starting at v2.03
                     , songName       :: [Word8]    -- 26 bytes
                     , hpad0          :: [Word8]    -- 2 bytes
                     , songLength     :: Word16     -- number of orders
                     , numInstruments :: Word16     -- number of instruments
                     , numSamples     :: Word16     -- number of samples
                     , numPatterns    :: Word16     -- number of patterns
                     , createdWith    :: Word16
                     , compatibleWith :: Word16
                     , flags          :: Word16     -- bit 0: stereo/mono
                                                    -- bit 1: no mixing occurs if volume
                                                    --        equals zero (deprecated)
                                                    -- bit 2: use samples/instruments
                                                    -- bit 3: linear/amiga slides
                                                    -- bit 4: use old effects
                                                    -- bits 5-15: undefined

                     , special        :: Word16     -- bit 0: message attached at messageOffset
                                                    -- bits 1-15: undefined
                     , globalVolume   :: Word8
                     , mixVolume      :: Word8
                     , initialSpeed   :: Word8
                     , initialTempo   :: Word8
                     , panSeparation  :: Word8
                     , hpad1          :: Word8
                     , messageLength  :: Word16
                     , messageOffset  :: Word32
                     , hpad2          :: [Word8]   -- 4 bytes
                     , channelPanning :: [Word8]
                     , channelVolume  :: [Word8]
                     }
    deriving (Show, Eq)

getHeader :: Get Header
getHeader = Header <$> getWord32le
                   <*> replicateM 26 getWord8
                   <*> replicateM 2 getWord8
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord16le
                   <*> getWord32le
                   <*> replicateM 4 getWord8
                   <*> replicateM 64 getWord8
                   <*> replicateM 64 getWord8

putHeader :: Header -> Put
putHeader Header{..} = do
    putWord32le magicNumber
    mapM_ putWord8 songName
    mapM_ putWord8 hpad0
    mapM_ putWord16le
          [ songLength
          , numInstruments
          , numSamples
          , numPatterns
          , createdWith
          , compatibleWith
          , flags
          , special
          ]
    mapM_ putWord8
          [ globalVolume
          , mixVolume
          , initialSpeed
          , initialTempo
          , panSeparation
          , hpad1
          ]
    putWord16le messageLength
    putWord32le messageOffset
    mapM_ putWord8 hpad2
    mapM_ putWord8 channelPanning
    mapM_ putWord8 channelVolume

