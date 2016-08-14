{-# LANGUAGE RecordWildCards #-}

module Data.Module.IT.Instrument (
      Instrument (..)
    , Envelope (..)
    , getInstrument
    , putInstrument
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.List.Split
import           Data.Tuple

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

getNode :: Get (Word16, Word8)
getNode = fmap swap (liftM2 (,) getWord8 getWord16le)

getEnvelope :: Get Envelope
getEnvelope = Envelope <$> getWord8 <*> getWord8 <*> getWord8
                       <*> getWord8 <*> getWord8 <*> getWord8
                       <*> replicateM 25 getNode <*> getWord8

putNode :: (Word16, Word8) -> Put
putNode (a,b) = putWord16le a >> putWord8 b

putEnvelope :: Envelope -> Put
putEnvelope Envelope{..} = do
    mapM_ putWord8 [ flag, numNodes, loopStart, loopEnd
                   , sustainLoopStart, sustainLoopEnd
                   ]
    mapM_ putNode nodes
    putWord8 epad0
    
data Instrument = Instrument { magicNumber           :: Word32             -- "IMPI"
                             , fileName              :: [Word8]            -- 12 bytes
                             , ipad0                 :: Word8
                             , newNoteAction         :: Word8
                             , duplicateCheckType    :: Word8
                             , duplicateCheckAction  :: Word8
                             , fadeOut               :: Word16
                             , pitchPanSeparation    :: Word8
                             , pitchPanCenter        :: Word8
                             , globalVolume          :: Word8
                             , defaultPan            :: Word8
                             , ipad1                 :: [Word8]            -- 2 bytes
                             , version               :: Word16
                             , sampleNum             :: Word8
                             , ipad2                 :: Word8
                             , name                  :: [Word8]            -- 26 bytes
                             , ipad3                 :: [Word8]            -- 6 bytes
                             , noteSampleTable       :: [(Word8, Word8)]   -- 240 bytes
                             , volumeEnvelope        :: Envelope
                             , panningEnvelope       :: Envelope
                             , pitchEnvelope         :: Envelope
                             }
    deriving (Show, Eq)

getInstrument :: Get Instrument
getInstrument = Instrument <$> getWord32le <*> replicateM 12 getWord8 <*> getWord8
                           <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord16le
                           <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
                           <*> replicateM 2 getWord8 <*> getWord16le <*> getWord8
                           <*> getWord8 <*> replicateM 26 getWord8 <*> replicateM 6 getWord8
                           <*> getNoteSampleTable <*> getEnvelope <*> getEnvelope <*> getEnvelope

getNoteSampleTable :: Get [(Word8, Word8)]
getNoteSampleTable = fmap (l2t . chunksOf 2) (replicateM 240 getWord8)
  where l2t = map (\[a,b] -> (a,b))

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

