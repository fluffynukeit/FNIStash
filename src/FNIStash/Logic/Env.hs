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
import Data.Int
import qualified Data.Map as M
import Database.HDBC.Sqlite3

-- Env is the lookup environment we pass around manually.  (I suppose we could use a Reader monad
-- but I tried it out and found it to be more complicated than simple argument passing)
data Env = Env
    { lkupEffect :: Word32 -> Maybe DATNode
    , lkupSkill :: T.Text -> Maybe DATNode
    , lkupLocNodes :: Word16 -> Word16 -> (DATNode, Maybe DATNode) -- location, containerID -> Container node, slot node
    , lkupLocIDs :: String -> String -> (Maybe Word16, Maybe Word16)
    , lkupItemGUID :: Int64 -> Maybe DATNode
    , lkupItemPath :: T.Text -> Maybe DATNode
    , totalItems :: Int
    , dbConn :: Connection
    }

-- build the lookup environment needed for app operations
buildEnv pak conn =
    let effects = effectLookup pak
        skills = skillLookup pak
        (bytesToNodesFxn, nodesToBytesFxn) = locLookup pak
        (itemsGUID, totalItems) = itemLookupGUID pak
        itemsPath = itemLookupPath pak
    in  Env effects skills bytesToNodesFxn nodesToBytesFxn itemsGUID itemsPath totalItems conn

-- Each of the functions below returns a lookup function.  This is how we can keep the loaded PAK
-- handy for repeated lookups since we cannot have a global.  The PAK stays on the stack.
itemLookupGUID pak =
    let guidFinder = \x -> fromJust $ lkupVar vUNIT_GUID x >>= stringVar >>= return . read
        dat = readDATFiles pak "MEDIA/UNITS/ITEMS" guidFinder -- p is pak
    in (\idInt64 -> lkupDATFile dat idInt64, M.size dat)

itemLookupPath pak =
    let ffp p = T.replace "\\" "/" p -- fix file path
    in \name -> lkupPAKFile (ffp name) pak >>= return . (runGetSuppress getDAT)

effectLookup pak =
    \effID -> lkupPAKFile "MEDIA/EFFECTSLIST.DAT" pak >>=
        return . (runGetSuppress getDAT) >>= subNodeAt effID

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
    -- ok, this is a little messy because Runic made an update in early April 2013 that changed
    -- how the slots are organized.  Originally they were all separate nodes in a INVENTORYSLOTS.DAT
    -- file, but now each slot is its own dat file.  I did as little as I needed to adapt the old
    -- algorithm to the new organizational scheme
    let invenSlotFiles = M.filterWithKey (\k _ -> T.isInfixOf "MEDIA/INVENTORY" k && not (T.isInfixOf "MEDIA/INVENTORY/CONTAINERS" k)) pak
        getName = \x -> lkupVar vNAME x >>= textVar
        slotsDatFiles = readDATFiles invenSlotFiles "MEDIA/INVENTORY" (fromJust . getName)
        allSlotTypesList = M.elems slotsDatFiles
        -- allSlotTypeList is a list of all Dat files for slots.  SlotDatFiles is a map of slot name
        -- to Dat file

        idFinder = \dat -> lkupVar vUNIQUEID dat >>= word32Var >>= \x -> return (fromIntegral x :: Word16)
        -- containers is a map of container ID to DATNode for the container
        containers = readDATFiles pak "MEDIA/INVENTORY/CONTAINERS" (fromJust . idFinder)

        -- make a search function for finding the slot type with unique ID closest but no greater
        -- than the locBytes Word16
        getID slotType = fromJust (lkupVar vUNIQUEID slotType >>= word32Var)
        winningSlot locBytes = priceIsRightSearch locBytes getID allSlotTypesList

        -- Now to piece it all together
        locBytesContIDToSlotCont locBytes contID =
            let cont = lkupDATFile containers contID
                slot = winningSlot locBytes
            in (slot, cont)

        -- Now create the reverse lookup: Given container and slot name, get container ID and slot ID
        slotNameToId :: String -> Maybe Word16
        slotNameToId name = M.lookup (T.pack name) slotsDatFiles >>= idFinder

        revContMap = M.fromList $ map (\(a,b) -> (fromJust $ getName b, a)) $ M.toList containers


        slotContToLocBytesContID slotName contName = (slotNameToId slotName, M.lookup (T.pack contName) revContMap)
    in (locBytesContIDToSlotCont, slotContToLocBytesContID)



