{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.List

import           Codec.Tracker.IT
import           Codec.Tracker.IT.Header
import           Codec.Tracker.IT.Instrument
import           Codec.Tracker.IT.Pattern

pprintInstrument :: Instrument -> IO ()
pprintInstrument Instrument{..} = do
    putStr $ "(" ++ show globalVolume ++ "): "
    BL.putStrLn $ BL.pack name

pprintHeader :: Header -> IO ()
pprintHeader Header{..} = do
    putStr "Song name....: "
    BL.putStrLn $ BL.pack songName
    putStrLn $ "Orders.......: " ++ show songLength
    putStrLn $ "Instruments..: " ++ show numInstruments
    putStrLn $ "Samples......: " ++ show numSamples
    putStrLn $ "Patterns.....: " ++ show numPatterns
    putStrLn $ "Global volume: " ++ show globalVolume
    putStrLn $ "Mix volume...: " ++ show mixVolume
    putStrLn $ "Initial speed: " ++ show initialSpeed
    putStrLn $ "Initial tempo: " ++ show initialTempo

pprintPattern :: Pattern -> IO ()
pprintPattern Pattern{..} = do
    putStrLn $ "Length: " ++ show patternLength ++ "  Rows: " ++ show numRows
    mapM_ putStrLn (map (foldr (++) ([])) (map (intersperse " | ") (map (map show) rows)))

main :: IO ()
main = do
    file <- BL.getContents
    let it = runGet getModule file
    putStrLn "Header:"
    putStrLn "======="
    pprintHeader $ header it
    putStrLn "<>"
    putStr   "Message: "
    BL.putStrLn $ BL.pack (message it)
    putStrLn "Instruments:"
    putStrLn "============"
    mapM_ pprintInstrument (instruments it)
    putStrLn "<>"
    putStrLn "Patterns:"
    putStrLn "========="
    mapM_ pprintPattern (patterns it)
    putStrLn "<>"
