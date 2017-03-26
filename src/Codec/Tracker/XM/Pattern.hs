{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase      #-}

module Codec.Tracker.XM.Pattern (
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
import           Data.Maybe

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


getCell :: Get Cell
getCell = getWord8 >>=
   \n -> if not (testBit n 7)
         then Cell (Just n) <$> (Just <$> getWord8) <*> (Just <$> getWord8)
                            <*> (Just <$> getWord8) <*> (Just <$> getWord8)
         else Cell <$> getByMask n 0 getWord8 <*> getByMask n 1 getWord8 
                   <*> getByMask n 2 getWord8 <*> getByMask n 3 getWord8
                   <*> getByMask n 4 getWord8

putCell :: Cell -> Put
putCell (Cell (Just n) Nothing Nothing Nothing Nothing) = putWord8 n
putCell Cell{..} = do
    putWord8 $ mask effectParam 4
             $ mask effectType  3
             $ mask volume      2
             $ mask instrument  1
             $ mask note        0
             $ setBit zeroBits  7
    maybe (return ()) putWord8 note
    maybe (return ()) putWord8 instrument
    maybe (return ()) putWord8 volume
    maybe (return ()) putWord8 effectType
    maybe (return ()) putWord8 effectParam
  where mask f n m = if isJust f then setBit m n else m

getPattern :: Get Pattern
getPattern = label "XM.Pattern" $ do
    headerLength <- getWord32le
    packingType <- getWord8
    numRows <- getWord16le
    packedSize <- getWord16le
    patternData <- getToLimit getCell packedSize
    return Pattern{..}

putPattern :: Pattern -> Put
putPattern Pattern{..} = do
    putWord32le headerLength
    putWord8 packingType
    mapM_ putWord16le [numRows, packedSize]
    mapM_ putCell patternData

