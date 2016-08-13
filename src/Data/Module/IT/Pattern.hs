{-# LANGUAGE RecordWildCards #-}

module Data.Module.IT.Pattern (
      Pattern (..)
    , getPattern
    , getEmptyPattern
    , putPattern
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.Maybe
import           Data.Traversable

data Command = Command { cmd :: Word8
                       , val :: Word8
                       }
    deriving (Show, Eq)

getCommand :: Get Command
getCommand = Command <$> getWord8 <*> getWord8

putCommand :: Command -> Put
putCommand Command{..} =
    putWord8 cmd >> putWord8 val

data Cell = Cell { channel    :: Word8
                 , _mask      :: Word8
                 , note       :: Maybe Word8
                 , instrument :: Maybe Word8
                 , volpan     :: Maybe Word8
                 , command    :: Maybe Command
                 }
    deriving (Show, Eq)

emptyCell :: Cell
emptyCell = Cell 0 0 Nothing Nothing Nothing Nothing

getCell :: Word8 -> Word8 -> [Cell] -> Get Cell
getCell channel mask rowBuffer = do
    n <- if testBit mask 4
         then pure (note lastCell)
         else sequence (if testBit mask 0 then Just getWord8 else Nothing)
    i <- if testBit mask 5
         then pure (instrument lastCell)
         else sequence (if testBit mask 1 then Just getWord8 else Nothing)
    v <- if testBit mask 6
         then pure (volpan lastCell)
         else sequence (if testBit mask 2 then Just getWord8 else Nothing)
    c <- if testBit mask 7
         then pure (command lastCell)
         else sequence (if testBit mask 3 then Just getCommand else Nothing)
    return $ Cell channel mask n i v c
  where lastCell = rowBuffer !! fromIntegral channel

data Pattern = Pattern { length  :: Word16
                       , numRows :: Word16
                       , ppad0   :: [Word8]           -- 4 bytes
                       , rows    :: [[Cell]]
                       }
    deriving (Show, Eq)

getPattern :: Get Pattern
getPattern = do
    length <- getWord16le
    numRows <- getWord16le
    ppad0 <- replicateM 4 getWord8
    rows <- liftM (map fst) (getRows (fromIntegral numRows))
    return Pattern{..}

getEmptyPattern :: Get Pattern
getEmptyPattern = return $ Pattern 0 64 [0, 0, 0, 0] (replicate 64 [])

putPattern :: Pattern -> Put
putPattern Pattern{..} = do
    putWord16le length
    putWord16le numRows
    mapM_ putWord8 ppad0
    putRows rows

replaceNth n new (x : xs)
    | n == 0 = new : xs
    | otherwise = x : replaceNth (n - 1) new xs

getRow :: [Cell] -> Get ([Cell], [Cell])
getRow rowBuffer = do
    chn <- getWord8
    if chn == 0
        then return ([], rowBuffer)
        else do
            let channel = (chn - 1) .&. 63
            mask <- if testBit chn 7
                    then getWord8
                    else pure (_mask (rowBuffer !! fromIntegral chn))
            c <- getCell channel mask rowBuffer
            j <- getRow (if testBit chn 7
                         then replaceNth (fromIntegral channel) c rowBuffer
                         else rowBuffer)
            return (fst j ++ [c], snd j)

getRows :: Int -> Get [([Cell], [Cell])]
getRows cnt
    | cnt <= 0 = pure [([], replicate 64 emptyCell)]
    | otherwise = do
          x <- getRows (cnt - 1)
          y <- getRow $ snd (last x)
          return $ x ++ [y]

putRows :: [[Cell]] -> Put
putRows _ = fail "not implemented"

