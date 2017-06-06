{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy         as BL
import           Data.Binary.Get
import           Data.Binary.Put

import           Codec.Tracker.S3M

main :: IO ()
main = do
  file <- BL.getContents
  let s3m0 = runGet getModule file
      s3m1 = runGet getModule $ runPut $ putModule s3m0
  if s3m0 == s3m1 then
    putStrLn "s3m0 and s3m1 are identical"
  else
    putStrLn "s3m0 and s3m1 differ"
