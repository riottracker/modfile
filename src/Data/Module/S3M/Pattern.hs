{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Data.Module.S3M.Pattern (
      Pattern (..)
    , Command (..)
    , Cell (..)
    , getPattern
    , putPattern
    ) where

import           Control.Applicative ((<$>))
import           Control.Monad
import           Control.Monad.Loops
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.Maybe
import           Data.Word

data Cell = Cell { mask       :: Word8
                 , note       :: Maybe Word8
                 , instrument :: Maybe Word8
                 , volume     :: Maybe Word8
                 , command    :: Maybe Command
                 }
    deriving (Show, Eq)

channel :: Cell -> Word8
channel = flip (foldl clearBit . mask) [5..7]

data Pattern = Pattern { packedLength  :: Word16
                       , rows          :: [[Cell]]
                       }
                   deriving (Show, Eq)

data Command = Command { cmd :: Word8
                       , val :: Word8
                       }
    deriving (Show, Eq)

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
        let getByMask b f = sequence $ if testBit mask b then Just f else Nothing
        n <- getByMask 5 getWord8
        i <- getByMask 5 getWord8
        v <- getByMask 6 getWord8
        c <- getByMask 7 getCommand
        return $ Just (Cell mask n i v c)

putCell :: Maybe Cell -> Put
putCell Nothing         = putWord8 0
putCell (Just Cell{..}) = putWord8 mask
                       >> maybe (return ()) putWord8 note
                       >> maybe (return ()) putWord8 instrument
                       >> maybe (return ()) putWord8 volume
                       >> maybe (return ()) putCommand command

getPattern :: Get Pattern
getPattern = label "S3M.Pattern" $ do
    br0 <- bytesRead
    packedLength <- getWord16le
    let getToLimit lst = bytesRead >>=
            \br1 -> if (br1 - br0) < fromIntegral packedLength then
                        getToLimit . (lst ++) . pure =<< getRow
                    else pure lst
    rows <- getToLimit []
    return Pattern{..}

getRow :: Get [Cell]
getRow = label "S3M.Pattern Row" $ whileJust getCell return

putRow :: [Cell] -> Put
putRow l = mapM_ (putCell . Just) l >> putCell Nothing

putPattern :: Pattern -> Put
putPattern Pattern{..} = putWord16le packedLength >> mapM_ putRow rows 

