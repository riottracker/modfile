{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.List
import           Text.Printf

import           Codec.Tracker.IT
import           Codec.Tracker.IT.Header
import           Codec.Tracker.IT.Instrument
import           Codec.Tracker.IT.Pattern

n2key :: Int -> String
n2key 255 = "###"
n2key 254 = "///"
n2key n   = ((cycle notes) !! n) ++ (show $ div n 12)
  where notes = ["C ","C#","D ","D#","E ","F ","F#","G ","G#","A ","A#","B "]

showCell :: Cell -> String
showCell Cell{..} = (maybe "---"     (printf "%3s" . n2key . fromIntegral) note) ++ " "
                 ++ (maybe ".."      (printf "%02X")                 instrument) ++ " "
                 ++ (maybe ".."      (printf "%02X")                     volpan) ++ " "
                 ++ (maybe "..."     printCommand                       command)
  where printCommand c = printf "%1X%02X" (cmd c) (val c)

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
    mapM_ putStrLn (map (foldr (++) ([])) (map (intersperse " | ") (map (map showCell) rows)))

main :: IO ()
main = do
    file <- BL.getContents
    let it = runGet getModule file
    putStrLn "Header:"
    putStrLn "======="
    pprintHeader $ header it
    putStrLn "<>"
    putStrLn "Instruments:"
    putStrLn "============"
    mapM_ pprintInstrument (instruments it)
    putStrLn "<>"
    putStrLn "Patterns:"
    putStrLn "========="
    mapM_ pprintPattern (patterns it)
    putStrLn "<>"
