-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.File.General
-- Copyright   :  2013 Daniel Austin
-- License     :  AllRightsReserved
--
-- Maintainer  :  dan@fluffynukeit.com
-- Stability   :  Development
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module FNIStash.File.General

where

import qualified Data.Binary.Strict.Get as S
import qualified Data.Binary.Get as L
import qualified Data.Text as T
import Control.Applicative
import Data.Text.Encoding (decodeUtf16LE)

getTorchText :: S.Get T.Text
getTorchText = fromIntegral . (*2) <$> S.getWord16le >>= S.getByteString >>= \x -> return (decodeUtf16LE x)

getTorchTextL :: L.Get T.Text
getTorchTextL = fromIntegral . (*2) <$> L.getWord16le >>= L.getByteString >>= \x -> return (decodeUtf16LE x)
