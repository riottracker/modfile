{-# LANGUAGE OverloadedStrings #-}

module Codec.Tracker.Format ( Format (..)
                            , detect
                            ) where

import qualified Data.ByteString.Lazy as B

data Format = XM | IT | ITInstrument | ITSample | S3M | MOD | Unknown

detect :: B.ByteString -> Format
detect b
  | B.take            4 b == "IMPM"     = IT
  | B.take            4 b == "IMPI"     = ITInstrument
  | B.take            4 b == "IMPS"     = ITSample
  | B.take            8 b == "Extended" = XM
  | (B.head $ B.drop 28 b) == 0x10      = S3M
  | otherwise                           = Unknown


