{-# LANGUAGE RecordWildCards #-}

-- | read/write ImpulseTracker samples
module Codec.Tracker.IT.Sample (
      SampleHeader (..)
    , getSampleHeader
    , putSampleHeader
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

-- | ImpulseTracker sample header
data SampleHeader = SampleHeader { magicNumber   :: Word32    -- ^ "IMPS"
                                 , fileName      :: [Word8]   -- ^ 12 bytes
                                 , spad0         :: Word8     -- ^ padding
                                 , globalVolume  :: Word8     -- ^ global volume (0-64)
                                 , flags         :: Word8     -- ^ bit 0: sample associated with header, bit 1: 16 bit/8 bit,
                                                              --   bit 2: stereo/mono, bit 3: sample compression, bit 4: loop,
                                                              --   bit 5: sustain loop, bit 6: ping-pong loop/forwards loop,
                                                              --   bit 7: ping-pong sustain loop/forwards sustain loop
                                 , defaultVolume :: Word8     -- ^ default volume
                                 , name          :: [Word8]   -- ^ sample name (26 bytes)
                                 , convert       :: Word16    -- ^ bit 0: signed/unsigned samples,
                                                              --   bit 1: intel lo-hi byte order/ motorola hi-lo byte order,
                                                              --   bit 2: pcm/delta values, bit 3: byte delta values,
                                                              --   bit 4: tx-wave 12-bit values
                                 , sampleLength  :: Word32    -- ^ sample length
                                 , loopBegin     :: Word32    -- ^ loop begin
                                 , loopEnd       :: Word32    -- ^ loop end
                                 , c5Speed       :: Word32    -- ^ tuning
                                 , susLoopBegin  :: Word32    -- ^ sustain loop begin
                                 , susLoopEnd    :: Word32    -- ^ sustain loop end
                                 , samplePointer :: Word32    -- ^ pointer to sample data
                                 , vibratoSpeed  :: Word8     -- ^ vibrato speed
                                 , vibratoDepth  :: Word8     -- ^ vibrato depth
                                 , vibratoRate   :: Word8     -- ^ vibrato rate
                                 , vibratoType   :: Word8     -- ^ vibrato waveform (0: sine, 1: ramp down, 2: square, 3: random)
-- cache file:                   , fileSize      :: Word32
--                               , date          :: Word16
--                               , time          :: Word16
--                               , format        :: Word8
--
                       }
    deriving (Show, Eq)

-- | Read a `SampleHeader` from the monad state.
getSampleHeader :: Get SampleHeader
getSampleHeader = label "IT.Sample SampleHeader" $
                  SampleHeader <$> getWord32le <*> replicateM 12 getWord8
                               <*> getWord8 <*> getWord8 <*> getWord8
                               <*> getWord8 <*> replicateM 26 getWord8
                               <*> getWord16le <*> getWord32le <*> getWord32le
                               <*> getWord32le <*> getWord32le <*> getWord32le
                               <*> getWord32le <*> getWord32le <*> getWord8
                               <*> getWord8 <*> getWord8 <*> getWord8

-- | Write a `SampleHeader` to the buffer.
putSampleHeader :: SampleHeader -> Put
putSampleHeader SampleHeader{..} = do
    putWord32le magicNumber
    mapM_ putWord8 (fileName ++ [spad0, globalVolume, flags, defaultVolume] ++ name)
    putWord16le convert
    mapM_ putWord32le [ sampleLength, loopBegin, loopEnd, c5Speed, susLoopBegin, susLoopEnd, samplePointer ]
    mapM_ putWord8    [ vibratoSpeed, vibratoDepth, vibratoRate, vibratoType ]

