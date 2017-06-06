module Main (main) where

import qualified Data.ByteString.Lazy         as BL
import           Data.Binary.Get
import           Data.Binary.Put

import           Codec.Tracker.XM

main :: IO ()
main = BL.putStrLn . runPut <$> putModule <$> runGet getModule =<< BL.getContents

