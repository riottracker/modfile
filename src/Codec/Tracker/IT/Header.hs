{-# LANGUAGE RecordWildCards #-}

-- | read/write ImpulseTracker file headers
module Codec.Tracker.IT.Header (
      Header (..)
    , getHeader
    , putHeader
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

-- | ImpulseTracker file header
data Header = Header { magicString    :: Word32     -- ^ \"IMPM\" starting at v2.03
                     , songName       :: [Word8]    -- ^ 26 bytes
                     , hpad0          :: [Word8]    -- ^ padding (2 bytes)
                     , songLength     :: Word16     -- ^ number of entries in pattern order table
                     , numInstruments :: Word16     -- ^ number of instruments
                     , numSamples     :: Word16     -- ^ number of samples
                     , numPatterns    :: Word16     -- ^ number of patterns
                     , trackerVersion :: Word16
                     , formatVersion  :: Word16
                     , flags          :: Word16     -- ^ bit 0: stereo/mono
                                                    --
                                                    --   bit 1: no mixing occurs if volume
                                                    --          equals zero (deprecated)
                                                    --
                                                    --   bit 2: use samples/instruments
                                                    --
                                                    --   bit 3: linear/amiga slides
                                                    --
                                                    --   bit 4: use old effects
                                                    --
                                                    --   bits 5-15: undefined

                     , special        :: Word16     -- ^ bit 0: there's message attached to the song
                                                    --   starting at `messageOffset`
                                                    --
                                                    --   bits 1-15: undefined
                     , globalVolume   :: Word8      -- ^ global volume
                     , mixVolume      :: Word8
                     , initialSpeed   :: Word8      -- ^ initial speed
                     , initialTempo   :: Word8      -- ^ initial tempo
                     , panSeparation  :: Word8
                     , hpad1          :: Word8      -- ^ padding
                     , messageLength  :: Word16
                     , messageOffset  :: Word32
                     , hpad2          :: [Word8]    -- ^ padding (4 bytes)
                     , channelPanning :: [Word8]
                     , channelVolume  :: [Word8]
                     }
    deriving (Show, Eq)

-- | Read a `Header` from the monad state.
getHeader :: Get Header
getHeader = label "IT.Header" $
            Header <$> getWord32le
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

-- | Write a `Header` to the buffer.
putHeader :: Header -> Put
putHeader Header{..} = do
    putWord32le magicString
    mapM_ putWord8 songName
    mapM_ putWord8 hpad0
    mapM_ putWord16le
          [ songLength
          , numInstruments
          , numSamples
          , numPatterns
          , trackerVersion
          , formatVersion
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


