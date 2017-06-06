{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString.Lazy as BL
import           Data.List
import           Text.Printf

import           Codec.Tracker.S3M
import           Codec.Tracker.S3M.Header
import           Codec.Tracker.S3M.Instrument
import           Codec.Tracker.S3M.Instrument.Adlib
import           Codec.Tracker.S3M.Instrument.PCM
import           Codec.Tracker.S3M.Pattern


pprintInstrument :: Instrument -> IO ()
pprintInstrument Instrument{..} = do
    BL.putStrLn $ BL.pack fileName
    forM_ pcmSample pprintPCMSample
    forM_ adlibSample pprintAdlibSample

pprintAdlibSample :: AdlibSample -> IO ()
pprintAdlibSample AdlibSample{..} = do
    putStr "Adlib: "
    BL.putStrLn $ BL.pack title

pprintPCMSample :: PCMSample -> IO ()
pprintPCMSample PCMSample{..} = do
   putStrLn "PCM: "
   BL.putStrLn $ BL.pack title

pprintHeader :: Header -> IO ()
pprintHeader Header{..} = do
    putStr     "Song name.......: "
    BL.putStrLn $ BL.pack songName
    putStrLn $ "Orders..........: " ++ show songLength
    putStrLn $ "Instruments.....: " ++ show numInstruments
    putStrLn $ "Patterns........: " ++ show numPatterns
    putStrLn $ "Version.........: " ++ show trackerVersion
    putStrLn $ "Global volume...: " ++ show globalVolume
    putStrLn $ "Initial speed...: " ++ show initialSpeed
    putStrLn $ "Initial tempo...: " ++ show initialTempo
    putStrLn $ "Mix volume......: " ++ show mixVolume
    putStrLn $ "Channel settings: " ++ show channelSettings

pprintPattern :: Pattern -> IO ()
pprintPattern Pattern{..} = do
    putStrLn $ "Packed length: " ++ show (packedSize Pattern{..})
    mapM_ putStrLn (map (foldr (++) ([])) (map (intersperse " | ") (map (map show) rows))) 

main :: IO ()
main = do
    file <- BL.getContents
    let s3m = runGet getModule file
    putStrLn "Header:"
    putStrLn "======="
    pprintHeader $ header s3m
    putStrLn "<>"
    print (orders s3m)
    putStrLn "<>"
    putStrLn "Instruments:"
    putStrLn "============"
    mapM_ pprintInstrument (instruments s3m)
    putStrLn "<>"
    putStrLn "Patterns:"
    putStrLn "========="
    mapM_ pprintPattern (patterns s3m)
    putStrLn "<>"
 
