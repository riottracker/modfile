{-# LANGUAGE RecordWildCards #-}

module Data.Module.XM.Pattern (
      Pattern (..)
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
data Pattern = Pattern { headerLength  :: Word64
                       , packingType   :: Word8
                       , numRows       :: Word32
                       , packedSize    :: Word32
                       , patternData   :: [(Word8, Word8, Word8, Word8, Word8)]
                       -- (note, instrument, volume, effect type, effect param)
                       }
                   deriving (Show, Eq)

getPatternData :: Int -> Get [(Word8, Word8, Word8, Word8, Word8)]
getPatternData size = do
    d <- replicateM size getWord8
    return $ map (\[a,b,c,d,e] -> (a,b,c,d,e)) (chunksOf 5 d)

getPattern :: Get Pattern
getPattern = do
    headerLength <- getWord64le
    packingType <- getWord8
    numRows <- getWord32le
    packedSize <- getWord32le
    patternData <- getPatternData $ fromIntegral packedSize 
    return Pattern{..}

putPattern :: Pattern -> Put
putPattern Pattern{..} = do
    putWord64le headerLength
    putWord8 packingType
    mapM_ putWord32le [numRows, packedSize]
    mapM_ putWord8 (foldr (\(a,b,c,d,e) f -> a : b : c : d : e : f) [] patternData)

