-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Env
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


module FNIStash.Logic.Env (
    buildEnv,
    Environment,
    module Control.Monad.Reader,
    Env (..)
) where

-- An ENV is the data environment that is passed around by the reader monad.  It has all the reference
-- data we need to do computations.

import FNIStash.File.PAK
import FNIStash.File.DAT
import FNIStash.File.Variables
import FNIStash.File.General

import qualified Data.Text as T
import Data.Maybe
import Data.Configurator
import Data.Binary.Get
import Data.Word
import Control.Monad.Reader

type Environment a = Reader Env a

data Env = Env {
    effects :: Word32 -> Maybe DATNode,
    skills :: T.Text -> Maybe DATNode
}

-- build the lookup environment needed for app operations
buildEnv cfg = do
    pakMANFileBinary <- require cfg "MANFILE"
    man <- readPAKMAN pakMANFileBinary
    let subMan = filterMANByPrefix man ["MEDIA/EFFECTSLIST.DAT","MEDIA/UNITS/ITEMS", "MEDIA/SKILLS"]
    pakFileBinary <- require cfg "PAKFILE"
    pak <- pakFiles subMan pakFileBinary
    return $ populateEnv pak

populateEnv pak =
    let effects = effectLookup pak
        skills = skillLookup pak
    in Env effects skills

-- this function returns a searching function.  I think this is the only way I can keep
-- the PAK and DAT maps instead of re-reading them each time I search
itemLookup pak =
    let guidFinder = (\x -> fromJust (findVar vUNIT_GUID x >>= textVar))
        dat = readDATFiles pak "MEDIA/UNITS/ITEMS" guidFinder -- p is pak
    in (\idText -> lkupDATFile dat idText)

effectLookup pak =
    let effListData = fromJust $ lkupPAKFile pak "MEDIA/EFFECTSLIST.DAT"
        dat = runGetSuppress getDAT effListData
    in (\effID -> sectionAt effID dat)

skillLookup pak =
    let nameFinder = (\x -> fromJust (findVar vNAME x >>= textVar))
        dat = readDATFiles pak "MEDIA/SKILLS" nameFinder -- p is pak
    in (\skillName -> lkupDATFile dat skillName)
