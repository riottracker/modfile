{-# LANGUAGE RecordWildCards #-}

module Data.Module.S3M.Header (
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
 
data Header = Header { songName        :: [Word8]    -- 28 bytes
                     , hpad0           :: Word8
                     , magicByte       :: Word8      -- should be 0x10 for scream tracker 3 modules
                     , hpad1           :: Word16
                     , songLength       :: Word16     -- number of entries in pattern order table
                     , numInstruments   :: Word16
                     , numPatterns      :: Word16
                     , flags            :: Word16
                     , trackerVersion   :: Word16
                     , sampleSignedness :: Word16
                     , magicString     :: Word32     -- "SCRM"
                     , globalVolume    :: Word8
                     , initialSpeed    :: Word8
                     , initialTempo    :: Word8
                     , mixVolume       :: Word8
                     , clickRemoval    :: Word8
                     , defaultPanFlag  :: Word8
                     , hpad2           :: [Word8]    -- 8 bytes
                     , special         :: Word16
                     , channelSettings :: [Word8]    -- 32 bytes/channels
                     }
    deriving (Show, Eq)

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

