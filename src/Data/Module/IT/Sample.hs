{-# LANGUAGE RecordWildCards #-}

module Data.Module.IT.Sample (
      SampleHeader (..)
    , getSampleHeader
    , putSampleHeader
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put

data SampleHeader = SampleHeader { magicNumber   :: Word64    -- "IMPS"
                                 , fileName      :: [Word8]   -- 12 bytes
                                 , spad0         :: Word8
                                 , globalVolume  :: Word8
                                 , flags         :: Word8
                                 , defaultVolume :: Word8
                                 , name          :: [Word8]   -- 26 bytes
                                 -- TODO
                       }
    deriving (Show, Eq)

getSampleHeader :: Get SampleHeader
getSampleHeader = SampleHeader <$> getWord64le <*> replicateM 12 getWord8
                               <*> getWord8 <*> getWord8 <*> getWord8
                               <*> getWord8 <*> replicateM 26 getWord8
                               -- TODO

putSampleHeader :: SampleHeader -> Put
putSampleHeader SampleHeader{..} = do
    putWord64le magicNumber
    mapM_ putWord8 (fileName ++ [spad0, globalVolume, flags, defaultVolume] ++ name)

