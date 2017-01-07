{-# LANGUAGE RecordWildCards #-}

module Data.Module.S3M.Instrument.Adlib (
      AdlibSample (..)
    , getAdlibSample
    , putAdlibSample
    ) where

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Control.Monad

data AdlibSample = AdlibSample { reserved0 :: [Word8] -- 3 bytes
                               , oplValues :: [Word8] -- 12 bytes
                               , volume    :: Word8
                               , dsk       :: Word8
                               , reserved1 :: Word16
                               , c2spd     :: Word32
                               , unused    :: [Word8] -- 12 bytes
                               , title     :: [Word8] -- 28 bytes
                               , sig       :: [Word8] -- 4 bytes, should be "SCRI"
                               }
    deriving (Show, Eq)

getAdlibSample :: Get AdlibSample
getAdlibSample = AdlibSample <$> replicateM 3 getWord8 <*> replicateM 12 getWord8 <*> getWord8
                             <*> getWord8 <*> getWord16le <*> getWord32le
                             <*> replicateM 12 getWord8 <*> replicateM 28 getWord8 <*> replicateM 4 getWord8

putAdlibSample :: AdlibSample -> Put
putAdlibSample AdlibSample{..} = do
    mapM_ putWord8 (reserved0 ++ oplValues ++ [volume, dsk])
    putWord16le reserved1
    putWord32le c2spd
    mapM_ putWord8 (unused ++ title ++ sig)

