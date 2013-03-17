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

import Debug.Trace

-- General stuff
import qualified Data.Text as T
import Data.Maybe
import Data.Configurator
import Data.Binary.Get
import Data.Word
import Control.Monad.Reader

type Environment a = Reader Env a


-- Env is the lookup environment we pass around using Environment a
data Env = Env
    { lkupEffect :: Word32 -> Maybe DATNode
    , lkupSkill :: T.Text -> Maybe DATNode
    , lkupLocNodes :: Word16 -> Word16 -> (Maybe DATNode, DATNode) -- location, containerID -> Container node, slot node
    }

-- build the lookup environment needed for app operations
buildEnv pak =
    let effects = effectLookup pak
        skills = skillLookup pak
        bytesToNodesFxn = locLookup pak
    in Env effects skills bytesToNodesFxn

-- Each of the functions below returns a lookup function.  This is how we can keep the loaded PAK
-- handy for repeated lookups since we cannot have a global.  The PAK stays on the stack.
itemLookup pak =
    let guidFinder = \x -> fromJust (lkupVar vUNIT_GUID x >>= textVar)
        dat = readDATFiles pak "MEDIA/UNITS/ITEMS" guidFinder -- p is pak
    in (\idText -> lkupDATFile dat idText)

effectLookup pak =
    let effListData = fromJust $ lkupPAKFile "MEDIA/EFFECTSLIST.DAT" pak
        dat = runGetSuppress getDAT effListData
    in (\effID -> subNodeAt effID dat)

skillLookup pak =
    let nameFinder = \x -> fromJust (lkupVar vNAME x >>= textVar)
        dat = readDATFiles pak "MEDIA/SKILLS" nameFinder
    in (\skillName -> lkupDATFile dat skillName)

priceIsRightSearch :: Word16 -> (DATNode -> Word32) -> [DATNode] -> DATNode
--priceIsRightSearch realPrice [] = who knows
priceIsRightSearch realPrice pricer (guess:guesses) =
    let i = fromIntegral
        startDiff = realPrice - (i . pricer) guess
        helper p f (diff, gBest) [] = gBest
        helper p f (diff, gBest) (g:gs) =
            if p-(f g) < diff && p-(f g) >= 0 then helper p f (p-(f g), g) gs else helper p f (diff, gBest) gs
    in helper realPrice (i . pricer) (startDiff, guess) guesses


locLookup pak =
    let allSlotTypesRoot = runGetSuppress getDAT $ fromJust $ lkupPAKFile "MEDIA/INVENTORY/INVENTORYSLOTS.DAT" pak
        allSlotTypesList = datSubNodes allSlotTypesRoot
        containerIDFinder = \dat -> fromJust (lkupVar vUNIQUEID dat >>= word32Var >>= \x -> return (fromIntegral x :: Word16))
        -- containers is a map of container ID to DATNode for the container
        containers = readDATFiles pak "MEDIA/INVENTORY/CONTAINERS" containerIDFinder

        -- make a search function for finding the slot type with unique ID closest but no greater
        -- than the locBytes Word16
        getID slotType = fromJust (lkupVar vUNIQUEID slotType >>= word32Var)
        winningSlot locBytes = priceIsRightSearch locBytes getID allSlotTypesList

        -- Now to piece it all together
        locBytesContIDToContSlot locBytes contID =
            let cont = lkupDATFile containers contID
                slot = winningSlot locBytes
            in (cont, slot)
    in locBytesContIDToContSlot



