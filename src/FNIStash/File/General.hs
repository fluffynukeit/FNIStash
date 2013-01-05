-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.File.General
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module FNIStash.File.General

where

import Data.Binary.Strict.Get
import qualified Data.Text as T
import Control.Applicative
import Data.Text.Encoding (decodeUtf16LE)

getTorchText :: Get T.Text
getTorchText = fromIntegral . (*2) <$> getWord16le >>= getByteString >>= \x -> return (decodeUtf16LE x)
