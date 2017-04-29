{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy         as BL

import           Data.Binary.Put

import           Codec.Tracker.S3M
import           Codec.Tracker.S3M.Header     as H
import           Codec.Tracker.S3M.Instrument as I

exampleHeader :: H.Header
exampleHeader = H.Header { H.songName   = (toEnum . fromEnum) <$> "Test"
                         , H.hpad0      = 0x1a
                         , H.magicByte  = 0x10
                         , H.hpad1      = 0
                         , H.songLength = 1
                         , H.numInstruments = 0
                         , H.numPatterns    = 0
                         , H.flags          = 0
                         , H.trackerVersion = 0x6666
                         , H.sampleSignedness = 2
                         , H.magicString      = 0x5243524d
                         , H.globalVolume     = 64
                         , H.initialSpeed     = 6
                         , H.initialTempo     = 125
                         , H.mixVolume        = 176
                         , H.clickRemoval     = 0
                         , H.defaultPanFlag   = 0
                         , H.hpad2            = replicate 8 0
                         , H.special          = 0
                         , H.channelSettings  = replicate 32 255
                         }

exampleSong :: Module
exampleSong = Module { header      = exampleHeader
                     , orders      = [0]
                     , insOffsets  = [0]
                     , patOffsets  = [0]
                     , panning     = replicate 32 0
                     , instruments = []
                     , patterns    = []
                     }

main :: IO ()
main = BL.putStrLn $ runPut $ putModule exampleSong
