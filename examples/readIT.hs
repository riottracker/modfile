{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad

import           Data.Module.IT
import           Data.Module.IT.Instrument
import           Data.Module.IT.Header
import           Data.Module.IT.Pattern

import           Data.Binary.Get

import qualified Data.ByteString.Lazy      as BL

pprintInstrument :: Instrument -> IO ()
pprintInstrument Instrument{..} = do
    putStr $ "(" ++ show globalVolume ++ "): "
    BL.putStrLn $ BL.pack name

pprintHeader :: Header -> IO ()
pprintHeader Header{..} = do
    putStr "Song name....: "
    BL.putStrLn $ BL.pack songName
    putStrLn $ "Orders.......: " ++ show ordNum
    putStrLn $ "Instruments..: " ++ show insNum
    putStrLn $ "Samples......: " ++ show smpNum
    putStrLn $ "Patterns.....: " ++ show patNum
    putStrLn $ "Global volume: " ++ show globalVolume
    putStrLn $ "Mix volume...: " ++ show mixVolume
    putStrLn $ "Initial speed: " ++ show initialSpeed
    putStrLn $ "Initial tempo: " ++ show initialTempo

pprintPattern :: Pattern -> IO ()
pprintPattern Pattern{..} = do
    putStrLn $ "Length: " ++ show length ++ "  Rows: " ++ show numRows
    print rows

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
