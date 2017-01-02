{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}

module Data.Module.XM.Pattern (
      Pattern (..)
    , getPattern
    , putPattern
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.List.Split
import           Data.Maybe (fromJust, isJust)

-- TODO: implement compression
data Pattern = Pattern { headerLength  :: Word32
                       , packingType   :: Word8
                       , numRows       :: Word16
                       , packedSize    :: Word16
                       , patternData   :: [(Word8, Word8, Word8, Word8, Word8)]
                       -- (note, instrument, volume, effect type, effect param)
                       }
                   deriving (Show, Eq)

getPatternData :: Int -> Get [(Word8, Word8, Word8, Word8, Word8)]
getPatternData size = do
    d <- replicateM size getWord8
    return . map (\[a,b,c,d,e] -> (a,b,c,d,e)) . filter ((== 5) . length) $ chunksOf 5 d

getPattern :: Get Pattern
getPattern = do
    headerLength <- getWord32le
    packingType <- getWord8
    numRows <- getWord16le
    packedSize <- getWord16le
    patternData <- getPatternData $ fromIntegral packedSize
    return Pattern{..}

putPattern :: Pattern -> Put
putPattern Pattern{..} = do
    putWord32le headerLength
    putWord8 packingType
    mapM_ putWord16le [numRows, packedSize]
    mapM_ putWord8 (foldr (\(a,b,c,d,e) f -> a : b : c : d : e : f) [] patternData)

