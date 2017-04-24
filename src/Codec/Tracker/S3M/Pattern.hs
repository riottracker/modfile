{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Codec.Tracker.S3M.Pattern (
      Pattern (..)
    , Command (..)
    , Cell (..)
    , Note (..)
    , channel
    , emptyCell
    , getPattern
    , putPattern
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad.Loops
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Text.Printf

import           Codec.Tracker.Common

import           Util


data Cell = Cell { mask       :: Word8
                 , note       :: Maybe Note
                 , instrument :: Maybe Word8
                 , volume     :: Maybe Word8
                 , command    :: Maybe Command
                 }
    deriving (Eq)

instance Enum Note where
  toEnum n
    | n < 254  = Note $ toEnum n
    | n == 254 = NoteCut
    | n == 255 = NoteOff
  fromEnum (Note p) = fromEnum p
  fromEnum NoteCut  = 254
  fromEnum NoteOff  = 255

instance Show Cell where
    show Cell{..} = (maybe "---" show                  note) ++ " "
                 ++ (maybe ".."  (printf "%02X") instrument) ++ " "
                 ++ (maybe ".."  (printf "%02X")     volume) ++ " "
                 ++ (maybe "..." show               command)

channel :: Cell -> Word8
channel = flip (foldl clearBit . mask) [5..7]

data Pattern = Pattern { packedLength  :: Word16
                       , rows          :: [[Cell]]
                       }
                   deriving (Show, Eq)

data Command = Command { cmd :: Word8
                       , val :: Word8
                       }
    deriving (Eq)

instance Show Command where
    show Command{..} = printf "%1X%02x" cmd val

getCommand :: Get Command
getCommand = label "S3M.Pattern Command" $
             Command <$> getWord8 <*> getWord8

putCommand :: Command -> Put
putCommand Command{..} =
    putWord8 cmd >> putWord8 val

emptyCell :: Cell
emptyCell = Cell 0 Nothing Nothing Nothing Nothing

getCell :: Get (Maybe Cell)
getCell = label "S3M.Pattern Cell" $
  getWord8 >>=
    \mask ->
      if mask == 0 then
          return Nothing
      else do
        n <- getByMask mask 5 (toEnum . fromIntegral <$> getWord8)
        i <- getByMask mask 5 getWord8
        v <- getByMask mask 6 getWord8
        c <- getByMask mask 7 getCommand
        return $ Just (Cell mask n i v c)

putCell :: Maybe Cell -> Put
putCell Nothing         = putWord8 0
putCell (Just Cell{..}) = putWord8 mask
                       >> maybe (return ()) putWord8 (toEnum . fromEnum <$> note)
                       >> maybe (return ()) putWord8 instrument
                       >> maybe (return ()) putWord8 volume
                       >> maybe (return ()) putCommand command

getPattern :: Get Pattern
getPattern = label "S3M.Pattern" $ do
    br0 <- bytesRead
    packedLength <- lookAhead getWord16le
    rows <- getToLimit getRow packedLength
    return Pattern{..}

getRow :: Get [Cell]
getRow = label "S3M.Pattern Row" $ whileJust getCell return

putRow :: [Cell] -> Put
putRow l = mapM_ (putCell . Just) l >> putCell Nothing

putPattern :: Pattern -> Put
putPattern Pattern{..} = putWord16le packedLength >> mapM_ putRow rows 

