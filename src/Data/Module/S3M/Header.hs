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
                     , numOrders       :: Word8
                     , numInstruments  :: Word8
                     , numPatterns     :: Word8
                     , flags           :: Word8
                     , trackerVersion  :: Word8
                     , format          :: Word8
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
getHeader = Header <$> replicateM 28 getWord8
                   <*> getWord8
                   <*> getWord8
                   <*> getWord16le
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8           
                   <*> getWord8
                   <*> getWord8
                   <*> getWord8
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
    putWord16le hpad1
    mapM_ putWord8
          [ numOrders
          , numInstruments
          , numPatterns
          , flags
          , trackerVersion
          , format
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

