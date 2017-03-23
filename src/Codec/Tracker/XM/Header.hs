{-# LANGUAGE RecordWildCards #-}

module Codec.Tracker.XM.Header (
      Header (..)
    , getHeader
    , putHeader
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Word

 
data Header = Header { idText          :: [Word8]    -- 17 bytes: "Extended module: "
                     , songName        :: [Word8]    -- 20 bytes
                     , hpad0           :: Word8
                     , trackerName     :: [Word8]    -- 20 bytes
                     , version         :: Word16
                     , headerSize      :: Word32
                     , songLength      :: Word16     -- number of entries in pattern order table
                     , restartPosition :: Word16
                     , numChannels     :: Word16
                     , numPatterns     :: Word16
                     , numInstruments  :: Word16
                     , flags           :: Word16    -- bit 0: use linear freq. table
                     , initialSpeed    :: Word16
                     , initialTempo    :: Word16
                     }
    deriving (Show, Eq)

getHeader :: Get Header
getHeader = label "XM.Header" $
            Header <$> replicateM 17 getWord8 
                   <*> replicateM 20 getWord8
                   <*> getWord8
                   <*> replicateM 20 getWord8
                   <*> getWord16le
                   <*> getWord32le
                   <*> getWord16le           
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le
                   <*> getWord16le

putHeader :: Header -> Put
putHeader Header{..} = do
    mapM_ putWord8 (idText ++ songName ++ [hpad0] ++ trackerName)
    putWord16le version
    putWord32le headerSize
    mapM_ putWord16le
          [ songLength
          , restartPosition
          , numChannels
          , numPatterns
          , numInstruments
          , flags
          , initialSpeed
          , initialTempo
          ]

