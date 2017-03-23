{-# LANGUAGE RecordWildCards #-}

{- This implementation will follow the documentation of .MOD files found at
 - http://www.fileformat.info/format/mod/corion.htm -}

module Data.Module.MOD (
    Module(..)
    , getModule
    , putModule
    ) where

import Control.Monad
import Data.Binary
import Data.Word
import Data.Binary.Get
import Data.Binary.Put

import Data.Module.MOD.Header
import Data.Module.MOD.Sample
import Data.Module.MOD.Pattern -- does not have content yet

data Module = Module { header   :: Header
                     , patterns :: [Pattern]
                     , samples  :: [Sample]
                     }

getModule :: Get Module
getModule = do
    header <- getHeader
    patterns <- replicateM_  (songLength header) getPattern -- something like this might work
    samples <- -- Use the information from the sample records in the header to do this
