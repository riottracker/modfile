{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Control.Monad

import           Data.Module.S3M
import           Data.Module.S3M.Header

import           Data.Binary.Get

import qualified Data.ByteString.Lazy      as BL

pprintHeader :: Header -> IO ()
pprintHeader Header{..} = do
    putStr     "Song name.......: "
    BL.putStrLn $ BL.pack songName
    putStrLn $ "Version.........: " ++ show trackerVersion
    putStrLn $ "Instruments.....: " ++ show numInstruments
    putStrLn $ "Orders..........: " ++ show numOrders
    putStrLn $ "Patterns........: " ++ show numPatterns
    putStrLn $ "Initial speed...: " ++ show initialSpeed
    putStrLn $ "Initial tempo...: " ++ show initialTempo
    putStrLn $ "Global volume...: " ++ show globalVolume
    putStrLn $ "Mix volume......: " ++ show mixVolume

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

