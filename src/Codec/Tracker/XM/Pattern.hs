{-# LANGUAGE RecordWildCards #-}

-- | read/write Fasttracker II patterns
module Codec.Tracker.XM.Pattern (
      Pattern (..)
    , Cell (..)
    , Note (..)
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
import           Text.Printf

import           Codec.Tracker.Common

import           Util


-- | Song event
data Cell = Cell { note        :: Maybe Note
                 , instrument  :: Maybe Word8
                 , volume      :: Maybe Word8
                 , effectType  :: Maybe Word8
                 , effectParam :: Maybe Word8
                 }
             deriving (Eq)


instance Enum Note where
  toEnum n
    | n > 0 && n <= 96 = Note $ toEnum (n - 1)
    | n == 97          = NoteOff
  fromEnum (Note p)    = 1 + fromEnum p
  fromEnum NoteOff     = 97

instance Show Cell where
    show Cell{..} = maybe "---" show                  note ++ " "
                 ++ maybe ".."  (printf "%02X") instrument ++ " "
                 ++ maybe ".."  (printf "%02X")     volume ++ " "
                 ++ maybe "."   (printf "%1X")  effectType
                 ++ maybe ".."  (printf "%2X") effectParam

-- | A Fasttracker II pattern
data Pattern = Pattern { headerLength  :: Word32
                       , packingType   :: Word8
                       , numRows       :: Word16
                       , packedSize    :: Word16
                       , patternData   :: [Cell]
                       }
                   deriving (Show, Eq)


-- | Read a `Cell` from the monad state.
getCell :: Get Cell
getCell = getWord8 >>=
   \n -> if not (testBit n 7)
         then Cell (Just . toEnum . fromIntegral $ n)
                            <$> (Just <$> getWord8) <*> (Just <$> getWord8)
                            <*> (Just <$> getWord8) <*> (Just <$> getWord8)
         else Cell <$> getByMask n 0 (toEnum . fromIntegral <$> getWord8)
                   <*> getByMask n 1 getWord8 <*> getByMask n 2 getWord8
                   <*> getByMask n 3 getWord8 <*> getByMask n 4 getWord8

-- | Write a `Cell` to the buffer.
putCell :: Cell -> Put
putCell (Cell (Just n) Nothing Nothing Nothing Nothing) = putWord8 (toEnum . fromEnum $ n)
putCell Cell{..} = do
    putWord8 $ mask effectParam 4
             $ mask effectType  3
             $ mask volume      2
             $ mask instrument  1
             $ mask note        0
             $ setBit zeroBits  7
    maybe (return ()) putWord8 (toEnum . fromEnum <$> note)
    maybe (return ()) putWord8 instrument
    maybe (return ()) putWord8 volume
    maybe (return ()) putWord8 effectType
    maybe (return ()) putWord8 effectParam
  where mask f n m = if isJust f then setBit m n else m

-- | Read a `Pattern` from the monad state.
getPattern :: Get Pattern
getPattern = label "XM.Pattern" $ do
    headerLength <- getWord32le
    packingType <- getWord8
    numRows <- getWord16le
    packedSize <- getWord16le
    patternData <- getToLimit getCell packedSize
    return Pattern{..}

-- | Write a `Pattern` to the buffer.
putPattern :: Pattern -> Put
putPattern Pattern{..} = do
    putWord32le headerLength
    putWord8 packingType
    mapM_ putWord16le [numRows, packedSize]
    mapM_ putCell patternData

