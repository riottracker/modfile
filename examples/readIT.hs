module Main (main) where

import Data.Module.IT

import Data.Binary.Get

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
    file <- BL.getContents
    print $ runGet getModule file
