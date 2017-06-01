{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BL

import           Codec.Tracker.XM

main :: IO ()
main = do
    file <- BL.getContents
    let xm0 = runGet getModule file
    let xm1 = runGet getModule $ runPut $ putModule xm0
    if xm0 == xm1 then
      putStrLn "xm0 and xm1 are identical"
    else
      putStrLn "xm0 and xm1 differ"
