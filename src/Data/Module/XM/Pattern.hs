{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}

module Data.Module.XM.Pattern (
      Pattern (..)
    , Cell (..)
    , getPattern
    , putPattern
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.List.Split
import           Data.Maybe (fromJust, isJust)
import           Data.Word

import           Util


data Cell = Cell { note        :: Maybe Word8
                 , instrument  :: Maybe Word8
                 , volume      :: Maybe Word8
                 , effectType  :: Maybe Word8
                 , effectParam :: Maybe Word8
                 }
             deriving (Show, Eq)

data Pattern = Pattern { headerLength  :: Word32
                       , packingType   :: Word8
                       , numRows       :: Word16
                       , packedSize    :: Word16
                       , patternData   :: [Cell]
                       }
                   deriving (Show, Eq)

-- getUncompressedPatternData :: Int -> Get [(Maybe Word8, Maybe Word8, Maybe Word8, Maybe Word8, Maybe Word8)]
-- getUncompressedPatternData size = label "XM.Pattern uncompressed row" $ do
--    d <- replicateM size getWord8
--    return . map (\[a,b,c,d,e] -> fmap Just (a,b,c,d,e)) . filter ((== 5) . length) $ chunksOf 5 d

getPatternData :: Int -> Get [Cell]
getPatternData size = label "XM.Pattern row" $ replicateM size getCell

getCell :: Get Cell
getCell = getWord8 >>=
      \n -> case testBit n 0 of
        False -> do
          i <- getWord8
          v <- getWord8
          et <- getWord8
          ep <- getWord8
          return $ Cell (Just n) (Just i) (Just v) (Just et) (Just ep)
        True  -> liftM5 Cell (getByMask n 0 getWord8) (getByMask n 1 getWord8) (getByMask n 2 getWord8) (getByMask n 3 getWord8) (getByMask n 4 getWord8)

getPattern :: Get Pattern
getPattern = label "XM.Pattern" $ do
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
    fail "not implemented"
--  mapM_ putWord8 (foldr (\(a,b,c,d,e) f -> a : b : c : d : e : f) [] patternData)

