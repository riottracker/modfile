{-# LANGUAGE RecordWildCards #-}

module Data.Module.S3M (
      Module (..)
    , getModule
    , putModule
    ) where

import           Control.Monad
import           Data.Binary
import           Data.Word
import           Data.Binary.Get
import           Data.Binary.Put


import Data.Module.S3M.Header

data Module = Module { header      :: Header
                     }
    deriving (Show, Eq)

getModule :: Get Module
getModule = do
    header <- getHeader
    return $ Module{..}

putModule :: Module -> Put
putModule Module{..} = do
    putHeader header

