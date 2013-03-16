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
-- data we need to do computations like lookups.

-- FNIStash stuff
import FNIStash.File.PAK
import FNIStash.File.DAT
import FNIStash.File.Variables
import FNIStash.File.General

-- General stuff
import qualified Data.Text as T
import Data.Maybe
import Data.Configurator
import Data.Binary.Get
import Data.Word
import Control.Monad.Reader

type Environment a = Reader Env a

-- Env is the lookup environment we pass around using Environment a
data Env = Env {
    effects :: Word32 -> Maybe DATNode,
    skills :: T.Text -> Maybe DATNode
}

-- build the lookup environment needed for app operations
buildEnv pak =
    let effects = effectLookup pak
        skills = skillLookup pak
    in Env effects skills

-- Each of hte function below returns a lookup function.  This is how we can keep the loaded PAK
-- handy for repeated lookups since we cannot have a global.  The PAK stays on the stack.
itemLookup pak =
    let guidFinder = (\x -> fromJust (findVar vUNIT_GUID x >>= textVar))
        dat = readDATFiles pak "MEDIA/UNITS/ITEMS" guidFinder -- p is pak
    in (\idText -> lkupDATFile dat idText)

effectLookup pak =
    let effListData = fromJust $ lkupPAKFile "MEDIA/EFFECTSLIST.DAT" pak
        dat = runGetSuppress getDAT effListData
    in (\effID -> sectionAt effID dat)

skillLookup pak =
    let nameFinder = (\x -> fromJust (findVar vNAME x >>= textVar))
        dat = readDATFiles pak "MEDIA/SKILLS" nameFinder
    in (\skillName -> lkupDATFile dat skillName)
