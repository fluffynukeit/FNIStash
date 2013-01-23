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
import Data.Binary.Get

import Data.Maybe

-- this function returns a searching function.  I think this is the only way I can keep
-- the PAK and DAT maps instead of re-reading them each time I search
itemSearcher pak = do
    let guidFinder = (\x -> fromJust (findVar vUNIT_GUID x >>= textVar))
    dat <- readDATFiles pak "MEDIA/UNITS/ITEMS" guidFinder -- p is pak
    return (\idText -> lkupDATFile dat idText)

effectLookup p = do
    effListData <- fromJust $ lkupPAKFile p "MEDIA/EFFECTLIST.DAT"
    let dat = runGet getDAT effListData
    return (\effID -> sectionAt effID dat)

