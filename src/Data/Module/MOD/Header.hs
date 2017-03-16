{-# LANGUAGE RecordWildCards LambdaCase #-}

module Data.Module.MOD.Header ( 
      Header (..)
    , getHeader
    , putHeader
    ) where

import Control.Applicative
import Control.Monad
import Data.Word
import Data.Module.MOD.Sample
import Data.Binary.Get

data Header = Header { songName    :: [Word8] -- 20 bytes, space- or zero-padded
                     , sampleRecs  :: [SampleRec] -- traditionally 15 samples, but mostly 31.
                     , songLength  :: Word8 -- Song length in patterns (up to 128)
                     , restartByte :: Word8 -- Dunno about this
                     -- 128 bytes, each normally contains numbers between 0 and 63.
                     , orderList   :: [Word8] 
                     -- 4 letters _may_ be present, they indicate capabilities
                     --   of the tracker and have some implications: 
                     , idLetters   :: Maybe [Word8]
                     }
    deriving (Show, Eq)

letterList = ["M.K.", "M!K!", "FLT4", "FLT8", "6CHN", "6CHN", "8CHN"]

letters :: [Word8] -> Maybe [Word8]
letters b
    | elem b letterList = Just b
    | otherwise = Nothing

getHeader :: Get Header
getHeader =  label "MOD.Header" $ do
    letterbytes <- lookAhead $ skip 1080 <*> replicateM 4 getWord8
    let idLetters = letters letterBytes
    let numOfSamples = case idLetters of Nothing -> 15
                                         _       -> 31
    songName <- replicateM 20 getWord8
    sampleRecs <- replicateM numOfSamples getSampleRec
    songLength <- getWord8
    restartByte <- getWord8
    orderList <- replicateM 128 getWord8
    return $ Header{..} 

putHeader :: Header -> Put
putHeader Header{..} = do 
    mapM_ putWord8 songName
    mapM_ putSampleRec sampleRecs
    mapM_ putWord8 [songLength, restartByte] ++ orderList
    unless (isNothing idLetters) (mapM_ putWord8 (fromJust idLetters))
