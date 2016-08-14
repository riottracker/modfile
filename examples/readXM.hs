{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad

import           Data.Module.XM
import           Data.Module.XM.Instrument
import           Data.Module.XM.Header
import           Data.Module.XM.Pattern

import           Data.Binary.Get

import qualified Data.ByteString.Lazy      as BL

pprintInstrument :: Instrument -> IO ()
pprintInstrument Instrument{..} = do
    putStr $ "(" ++ show sampleNum ++ "): "
    BL.putStrLn $ BL.pack name

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

pprintPattern :: Pattern -> IO ()
pprintPattern Pattern{..} = do
    putStrLn $ "Packed size: " ++ show packedSize ++ "  Rows: " ++ show numRows
    print $ patternData

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
