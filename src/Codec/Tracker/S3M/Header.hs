{-# LANGUAGE RecordWildCards #-}

-- | read/write Scream Tracker 3 file headers
module Codec.Tracker.S3M.Header (
      Header (..)
    , getHeader
    , putHeader
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

-- | Scream Tracker 3 file header 
data Header = Header { songName        :: [Word8]    -- ^ song name (28 bytes)
                     , hpad0           :: Word8      -- ^ should be 0x1a for scream tracker 3 modules
                     , magicByte       :: Word8      -- ^ should be 0x10 for scream tracker 3 modules
                     , hpad1           :: Word16     -- ^ padding
                     , songLength       :: Word16    -- ^ number of entries in pattern order table
                     , numInstruments   :: Word16    -- ^ number of instruments
                     , numPatterns      :: Word16    -- ^ number of patterns
                     , flags            :: Word16    -- ^ bit 0: scream tracker 2 vibrato, bit 1: scream tracker 2 tempo,
                                                     --   bit 2: amiga slides, bit 3: turn of looping notes when volume is zero for more than two rows,
                                                     --   bit 4: amiga limits, bit 5: soundblaster filter/sfx, bit 6: scream tracker 3 volume slides
                     , trackerVersion   :: Word16    -- ^ upper four bits: tracker id, lower 12 bits: version
                     , sampleSignedness :: Word16    -- ^ 1: signed, 2: unsigned
                     , magicString     :: Word32     -- ^ \"SCRM\"
                     , globalVolume    :: Word8      -- ^ global volume
                     , initialSpeed    :: Word8      -- ^ frames per row
                     , initialTempo    :: Word8      -- ^ frames per second
                     , mixVolume       :: Word8      -- ^ bit 7: 1=stereo, 0=mono
                                                     -- 
                                                     --   bits 6-0: volume
                     , clickRemoval    :: Word8      -- ^ number of channels to use for click removal on real GUS hardware
                     , defaultPanFlag  :: Word8      -- ^ if 252: read panning values
                     , hpad2           :: [Word8]    -- ^ padding (8 bytes)
                     , special         :: Word16     -- ^ unused
                     , channelSettings :: [Word8]    -- ^ channel settings (32 bytes/channels)
                     }
    deriving (Show, Eq)

-- | Read a `Header` from the monad state.
getHeader :: Get Header
getHeader = label "S3M.Header" $
            Header <$> replicateM 28 getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord32le
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> replicateM 8 getWord8
                   <*> getWord16le
                   <*> replicateM 32 getWord8

-- | Write a `Header` to the buffer.
putHeader :: Header -> Put
putHeader Header{..} = do
    mapM_ putWord8 (songName ++ [hpad0, magicByte])
    mapM_ putWord16le
          [ hpad1
          , songLength
          , numInstruments
          , numPatterns
          , flags
          , trackerVersion
          , sampleSignedness
          ]
    putWord32le magicString
    mapM_ putWord8 $
          [ globalVolume
          , initialSpeed
          , initialTempo
          , mixVolume
          , clickRemoval
          , defaultPanFlag
          ] ++ hpad2
    putWord16le special
    mapM_ putWord8 channelSettings

