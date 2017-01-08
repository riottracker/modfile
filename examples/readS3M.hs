{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad

import           Data.Module.S3M
import           Data.Module.S3M.Instrument
import           Data.Module.S3M.Header
import           Data.Module.S3M.Pattern

import           Data.Binary.Get

import qualified Data.ByteString.Lazy      as BL

pprintInstrument :: Instrument -> IO ()
pprintInstrument Instrument{..} = do
    BL.putStr $ BL.pack filename

pprintHeader :: Header -> IO ()
pprintHeader Header{..} = do
    putStr     "Song name.......: "
    BL.putStrLn $ BL.pack songName
    putStrLn $ "Orders..........: " ++ show numOrders
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
    putStrLn $ "Packed length: " ++ show packedLength

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
