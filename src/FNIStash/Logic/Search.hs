-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Search
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
{-# LANGUAGE OverloadedStrings #-}

module FNIStash.Logic.Search (
    itemSearcher
) where

import FNIStash.File.PAK
import FNIStash.File.DAT
import FNIStash.Logic.Variables
import qualified Data.Text as T

import Data.Maybe

-- this function returns a searching function.  I think this is the only way I can keep
-- the PAK and DAT maps instead of re-reading them each time I search
itemSearcher p = do
    let guidFinder = (\x -> fromJust (findVar vUNIT_GUID x >>= textVar))
    d <- readDATFiles p "MEDIA/UNITS/ITEMS" guidFinder
    return (\idText -> lkupDATFile d idText)

