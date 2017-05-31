{-# LANGUAGE RecordWildCards, TupleSections #-}

-- | read/write Fasttracker II instruments
module Codec.Tracker.XM.Instrument (
      Instrument (..)
    , ExtendedInstrumentHeader (..)
    , getInstrument
    , putInstrument
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put

import           Codec.Tracker.XM.SampleHeader

-- | Instrument
data Instrument = Instrument { instrumentSize :: Word32
                             , instrumentName :: [Word8]                         -- ^ 22 bytes
                             , instrumentType :: Word8                           -- ^ always zero?
                             , sampleNum      :: Word16                          -- ^ number of samples used
                             , extendedHeader :: Maybe ExtendedInstrumentHeader  -- ^ if numSamples > 0
                             , samples        :: [(SampleHeader, [Word8])]
                             }
    deriving (Show, Eq)

-- | Used if there is at least one sample associated with the `Instrument`.
data ExtendedInstrumentHeader = ExtendedInstrumentHeader { sampleHeaderSize :: Word32
                                                         , keymap           :: [Word8]
                                                         , volumeEnvelope   :: [Word8]
                                                         , panningEnvelope  :: [Word8]
                                                         , volNumNodes      :: Word8
                                                         , panNumNodes      :: Word8
                                                         , panSustainNode   :: Word8
                                                         , volLoopStart     :: Word8
                                                         , volLoopEnd       :: Word8
                                                         , panSustainPoint  :: Word8
                                                         , panLoopStart     :: Word8
                                                         , panLoopEnd       :: Word8
                                                         , volumeType       :: Word8
                                                         , panningType      :: Word8
                                                         , vibratoType      :: Word8
                                                         , vibratoSweep     :: Word8
                                                         , vibratoDepth     :: Word8
                                                         , vibratoRate      :: Word8
                                                         , volumeFadeOut    :: Word16
                                                         , reserved         :: Word16
                                                         }
                                                       deriving (Show, Eq)

-- | Read an `ExtendedInstrumentHeader` from the monad state.
getExtendedInstrumentHeader :: Get ExtendedInstrumentHeader
getExtendedInstrumentHeader = ExtendedInstrumentHeader <$> getWord32le <*> replicateM 96 getWord8
                                                       <*> replicateM 48 getWord8 <*> replicateM 48 getWord8
                                                       <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
                                                       <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
                                                       <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
                                                       <*> getWord8 <*> getWord8 <*> getWord16le <*> getWord16le

-- | Read an `Instrument` from the monad state.
getInstrument :: Get Instrument
getInstrument = label "XM.Instrument" $ do
    headerStart <- bytesRead
    instrumentSize <- getWord32le
    instrumentName <- replicateM 22 getWord8
    instrumentType <- getWord8
    sampleNum <- getWord16le
    extendedHeader <- sequence $ if sampleNum > 0 then Just getExtendedInstrumentHeader else Nothing
    headerEnd <- bytesRead
    skip $ fromIntegral instrumentSize + fromIntegral headerStart - fromIntegral headerEnd
    sampleHeaders <- replicateM (fromIntegral sampleNum) getSampleHeader
    samples <- mapM getSample sampleHeaders
    return Instrument{..}

-- | Read the sample data for a `SampleHeader` and return a pair.
getSample :: SampleHeader -> Get (SampleHeader, [Word8])
getSample h = fmap (h ,) (replicateM (fromIntegral (sampleLength h)) getWord8)

-- | Write an `Instrument` to the buffer.
putInstrument :: Instrument -> Put
putInstrument Instrument{..} = do
    putWord32le instrumentSize
    mapM_ putWord8 instrumentName
    putWord8 instrumentType
    putWord16le sampleNum
    mapM_ putExtendedInstrumentHeader extendedHeader 
    mapM_ (putSampleHeader . fst) samples
    mapM_ (mapM_ putWord8 . snd) samples

-- | Write an `ExtendedInstrumentHeader` to the buffer.
putExtendedInstrumentHeader :: ExtendedInstrumentHeader -> Put
putExtendedInstrumentHeader ExtendedInstrumentHeader{..} = do
    putWord32le sampleHeaderSize
    mapM_ putWord8 keymap
    mapM_ putWord8 volumeEnvelope
    mapM_ putWord8 panningEnvelope
    mapM_ putWord8 [ volNumNodes, panNumNodes, panSustainNode, volLoopStart
                   , volLoopEnd, panSustainPoint, panLoopStart, panLoopEnd
                   , volumeType, panningType, vibratoType, vibratoSweep
                   , vibratoDepth, vibratoRate ]
    mapM_ putWord16le [ volumeFadeOut, reserved ]

