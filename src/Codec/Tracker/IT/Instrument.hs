{-# LANGUAGE RecordWildCards #-}

-- | read/write ImpulseTracker imstruments
module Codec.Tracker.IT.Instrument (
      Instrument (..)
    , Envelope (..)
    , getInstrument
    , putInstrument
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.List.Split
import           Data.Tuple

-- | Envelope given by a list of nodes.
data Envelope = Envelope { flag             :: Word8
                         , numNodes         :: Word8
                         , loopStart        :: Word8
                         , loopEnd          :: Word8
                         , sustainLoopStart :: Word8
                         , sustainLoopEnd   :: Word8
                         , nodes            :: [(Word16, Word8)] -- 75 bytes
                         , epad0            :: Word8
                         }
    deriving (Show, Eq)

-- | Impulsetracker instrument
data Instrument = Instrument { magicNumber           :: Word32             -- ^ \"IMPI\"
                             , fileName              :: [Word8]            -- ^ 12 bytes
                             , ipad0                 :: Word8              -- ^ padding
                             , newNoteAction         :: Word8              -- ^ New note action:
                                                                           --
                                                                           --   0: cut,
                                                                           --   1: continue,
                                                                           --   2: note off,
                                                                           --   3: note fade

                             , duplicateCheckType    :: Word8              -- ^ Duplicate check type:
                                                                           --
                                                                           --   0: off,
                                                                           --   1: note,
                                                                           --   2: sample,
                                                                           --   3: instrument

                             , duplicateCheckAction  :: Word8              -- ^ Duplicate check action
                                                                           --
                                                                           --   0: cut,
                                                                           --   1: note off,
                                                                           --   2: note fade

                             , fadeOut               :: Word16
                             , pitchPanSeparation    :: Word8
                             , pitchPanCenter        :: Word8
                             , globalVolume          :: Word8
                             , defaultPan            :: Word8
                             , ipad1                 :: [Word8]            -- ^ padding (2 bytes)
                             , version               :: Word16
                             , sampleNum             :: Word8
                             , ipad2                 :: Word8
                             , name                  :: [Word8]            -- 26 bytes
                             , ipad3                 :: [Word8]            -- ^ padding (6 bytes)
                             , noteSampleTable       :: [(Word8, Word8)]   -- 240 bytes
                             , volumeEnvelope        :: Envelope
                             , panningEnvelope       :: Envelope
                             , pitchEnvelope         :: Envelope
                             }
    deriving (Show, Eq)

-- | Read a single envelope node from the monad state.
getNode :: Get (Word16, Word8)
getNode = label "IT.Instrument Node" $
          fmap swap (liftM2 (,) getWord8 getWord16le)

-- | Read an `Envelope` from the monad state.
getEnvelope :: Get Envelope
getEnvelope = label "IT.Instrument Envelope" $
              Envelope <$> getWord8 <*> getWord8 <*> getWord8
                       <*> getWord8 <*> getWord8 <*> getWord8
                       <*> replicateM 25 getNode <*> getWord8

-- | Write single envelope node to the buffer.
putNode :: (Word16, Word8) -> Put
putNode (a,b) = putWord16le a >> putWord8 b

-- | Write an `Envelope` to the buffer.
putEnvelope :: Envelope -> Put
putEnvelope Envelope{..} = do
    mapM_ putWord8 [ flag, numNodes, loopStart, loopEnd
                   , sustainLoopStart, sustainLoopEnd
                   ]
    mapM_ putNode nodes
    putWord8 epad0

-- | Read an `Instrument` from the monad state.
getInstrument :: Get Instrument
getInstrument = label "IT.Instrument" $
                Instrument <$> getWord32le <*> replicateM 12 getWord8 <*> getWord8
                           <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord16le
                           <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
                           <*> replicateM 2 getWord8 <*> getWord16le <*> getWord8
                           <*> getWord8 <*> replicateM 26 getWord8 <*> replicateM 6 getWord8
                           <*> getNoteSampleTable <*> getEnvelope <*> getEnvelope <*> getEnvelope

-- | Read a set of (key, sample) mappings from the monad state.
getNoteSampleTable :: Get [(Word8, Word8)]
getNoteSampleTable = label "IT.Instrument NoteSampleTable" $
                     fmap (l2t . chunksOf 2) (replicateM 240 getWord8)
  where l2t = map (\[a,b] -> (a,b))


-- | Write an `Instrument` to the buffer.
putInstrument :: Instrument -> Put
putInstrument Instrument{..} = do
    putWord32le magicNumber
    mapM_ putWord8 fileName
    mapM_ putWord8
          [ ipad0
          , newNoteAction
          , duplicateCheckType
          , duplicateCheckAction
          ]
    putWord16le fadeOut
    mapM_ putWord8
          [ pitchPanSeparation
          , pitchPanCenter
          , globalVolume
          , defaultPan
          ]
    mapM_ putWord8 ipad1
    putWord16le version
    mapM_ putWord8 ([sampleNum, ipad2] ++ name ++ ipad3)
    mapM_ putWord8 (foldr (\(f,s) a -> f : s : a) [] noteSampleTable)
    mapM_ putEnvelope
          [ volumeEnvelope
          , panningEnvelope
          , pitchEnvelope
          ]

