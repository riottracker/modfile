{-# LANGUAGE RecordWildCards #-}

module Data.Module.XM.Pattern (
      Pattern (..)
    , PatternHeader (..)
    , getPattern
    , putPattern
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.List.Split

-- TODO: implement compression
data Pattern = Pattern { patternHeader :: PatternHeader
                       , patternData   :: [(Word8, Word8, Word8, Word8, Word8)]
                       -- (note, instrument, volume, effect type, effect param)
                       }
                   deriving (Show, Eq)

data PatternHeader = PatternHeader { headerLength :: Word64
                                   , packingType  :: Word8
                                   , numRows      :: Word32
                                   , packedSize   :: Word32
                                   }
                               deriving (Show, Eq)

getPatternData :: Int -> Get [(Word8, Word8, Word8, Word8, Word8)]
getPatternData size = do
    d <- replicateM size getWord8
    return $ map (\[a,b,c,d,e] -> (a,b,c,d,e)) (chunksOf 5 d)

getPattern :: Get Pattern
getPattern = do
    patternHeader <- getPatternHeader
    patternData <- getPatternData $ fromIntegral (packedSize patternHeader)
    return $ Pattern{..}

putPattern :: Pattern -> Put
putPattern Pattern{..} = do
    putPatternHeader patternHeader
    mapM_ putWord8 (foldr (\(a,b,c,d,e) f -> a : b : c : d : e : f) [] patternData)

getPatternHeader :: Get PatternHeader
getPatternHeader = PatternHeader <$> getWord64le <*> getWord8
                                 <*> getWord32le <*> getWord32le

putPatternHeader :: PatternHeader -> Put
putPatternHeader PatternHeader{..} = do
    putWord64le headerLength
    putWord8 packingType
    mapM_ putWord32le [numRows, packedSize]

