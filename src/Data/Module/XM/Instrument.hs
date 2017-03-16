{-# LANGUAGE RecordWildCards #-}

module Data.Module.XM.Instrument (
      Instrument (..)
    , ExtendedInstrumentHeader (..)
    , getInstrument
    , putInstrument
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Word

import           Data.Module.XM.Sample


data Instrument = Instrument { instrumentSize :: Word32
                             , instrumentName :: [Word8]    -- 22 bytes
                             , instrumentType :: Word8
                             , sampleNum      :: Word16
-- if numSamples > 0
                             , extendedHeader :: Maybe ExtendedInstrumentHeader
                             , samples        :: Maybe [Sample]
                             }
    deriving (Show, Eq)

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

getExtendedInstrumentHeader :: Get ExtendedInstrumentHeader
getExtendedInstrumentHeader = ExtendedInstrumentHeader <$> getWord32le <*> replicateM 96 getWord8
                                                       <*> replicateM 48 getWord8 <*> replicateM 48 getWord8
                                                       <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
                                                       <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
                                                       <*> getWord8 <*> getWord8 <*> getWord8 <*> getWord8
                                                       <*> getWord8 <*> getWord8 <*> getWord16le <*> getWord16le

getInstrument :: Get Instrument
getInstrument = label "XM.Instrument" $ do
     headerStart <- bytesRead
     instrumentSize <- getWord32le
     instrumentName <- replicateM 22 getWord8
     instrumentType <- getWord8
     sampleNum <- getWord16le
     extendedHeader <- sequence $ if sampleNum > 0 then Just getExtendedInstrumentHeader else Nothing
     headerEnd <- bytesRead
     skip $ (fromIntegral instrumentSize) + (fromIntegral headerStart) - (fromIntegral headerEnd)
     samples <- sequence $ if sampleNum > 0 then Just (replicateM (fromIntegral sampleNum) getSample) else Nothing
     return Instrument{..}

putInstrument :: Instrument -> Put
putInstrument Instrument{..} = do
    putWord32le instrumentSize
    mapM_ putWord8 instrumentName
    putWord8 instrumentType
    putWord16le sampleNum
-- TODO

