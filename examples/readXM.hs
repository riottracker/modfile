{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.List
import           Data.List.Split
import           Text.Printf

import           Codec.Tracker.XM
import           Codec.Tracker.XM.Header
import           Codec.Tracker.XM.Instrument
import           Codec.Tracker.XM.Pattern

n2key :: Int -> String
n2key 97 = "###"
n2key n  = ((cycle notes) !! (n - 1)) ++ (show $ div n 12)
  where notes = ["C ","C#","D ","D#","E ","F ","F#","G ","G#","A ","A#","B "]

pprintInstrument :: Instrument -> IO ()
pprintInstrument Instrument{..} = do
    BL.putStr $ BL.pack instrumentName
    putStrLn $ " (" ++ show sampleNum ++ ")"

pprintHeader :: Header -> IO ()
pprintHeader Header{..} = do
    putStr     "Song name.......: "
    BL.putStrLn $ BL.pack songName
    putStr     "Tracker name....: "
    BL.putStrLn $ BL.pack trackerName
    putStrLn $ "Version.........: " ++ show version
    putStrLn $ "Orders..........: " ++ show songLength
    putStrLn $ "Restart position: " ++ show restartPosition
    putStrLn $ "Instruments.....: " ++ show numInstruments
    putStrLn $ "Channels........: " ++ show numChannels
    putStrLn $ "Patterns........: " ++ show numPatterns
    putStrLn $ "Initial speed...: " ++ show initialSpeed
    putStrLn $ "Initial tempo...: " ++ show initialTempo

showCell :: Cell -> String
showCell Cell{..} = (maybe "---" (printf "%3s" . n2key . fromIntegral) note) ++ " "
                 ++ (maybe ".."  (printf "%02X")                 instrument) ++ " "
                 ++ (maybe ".."  (printf "%02X")                     volume) ++ " "
                 ++ (maybe "."   (printf "%1X")                  effectType)
                 ++ (maybe ".."  (printf "%2X")                 effectParam)

pprintPattern :: Int -> Pattern -> IO ()
pprintPattern n Pattern{..} = do
    putStrLn $ "Packed size: " ++ show packedSize ++ "  Rows: " ++ show numRows
    mapM_ putStrLn (map (foldr (++) ([])) (map (intersperse " | ") (chunksOf n (map showCell patternData))))


main :: IO ()
main = do
    file <- BL.getContents
    let xm = runGet getModule file
    putStrLn "Header:"
    putStrLn "======="
    pprintHeader $ header xm
    putStrLn "<>"
    print (orders xm)
    putStrLn "<>"
    putStrLn "Instruments:"
    putStrLn "============"
    mapM_ pprintInstrument (instruments xm)
    putStrLn "<>"
    putStrLn "Patterns:"
    putStrLn "========="
    mapM_ (pprintPattern $ fromIntegral . numChannels . header $ xm) (patterns xm)
    putStrLn "<>"
