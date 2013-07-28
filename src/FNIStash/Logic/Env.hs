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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module FNIStash.Logic.Env 
    ( buildEnv
    , searchAncestryFor
    , Env (..)
    , EffectKey (..)
    ) where

-- An ENV is the data environment that is passed around by the reader monad.  It has all the reference
-- data we need to do computations like lookups.

-- FNIStash stuff
import FNIStash.File.PAK
import FNIStash.File.DAT
import FNIStash.File.Variables
import FNIStash.File.General
import FNIStash.File.Item (LocationBytes(..))

-- General stuff
import qualified Data.Text as T
import Data.Maybe
import Data.Configurator
import Data.Binary.Get
import Data.Word
import Data.Int
import qualified Data.Map as M
import qualified Data.List as L
import Database.HDBC.Sqlite3

import Debug.Trace

-- Env is the lookup environment we pass around manually.  (I suppose we could use a Reader monad
-- but I tried it out and found it to be more complicated than simple argument passing)
data Env = Env
    { lkupEffect :: EffectKey -> Maybe DATNode
    , lkupAffix :: T.Text -> Maybe DATNode
    , lkupSkill :: T.Text -> Maybe DATNode
    , lkupMonster :: T.Text -> Maybe DATNode
    , lkupLocNodes :: LocationBytes -> (DATNode, Maybe DATNode) -- location, containerID -> Container node, slot node
    , lkupLocIDs :: String -> String -> (Maybe SlotID, Maybe ContainerID)
    , lkupItemGUID :: GUID -> Maybe DATNode
    , lkupTriggerable :: T.Text -> Maybe DATNode
    , lkupStat :: T.Text -> Maybe DATNode
    , lkupPath :: T.Text -> Maybe DATNode
    , lkupGraph :: T.Text -> Float -> Float
    , allItems :: DATFiles GUID -- map of GUID to item nodes
    , dbConn :: Connection
    }

data EffectKey = EffectIndex
    { effectIndexVal :: Word32
    }
    | EffectName
    { effectName :: String
    } deriving (Eq, Ord)


-- build the lookup environment needed for app operations
buildEnv pak conn =
    let effects = effectLookup pak
        skills = skillLookup pak
        (bytesToNodesFxn, nodesToBytesFxn) = locLookup pak
        (itemsGUID, allItemsMap) = itemLookupGUID pak
        byPath = lookupPath pak
        graph = graphLookup byPath
        affixes = affixLookup pak
        monsters = monsterLookup pak
        trigs = triggerableLookup pak
        stats = statLookup pak
    in  Env effects affixes skills monsters bytesToNodesFxn
            nodesToBytesFxn itemsGUID trigs stats byPath graph
            allItemsMap conn

-- Each of the functions below returns a lookup function.  This is how we can keep the loaded PAK
-- handy for repeated lookups since we cannot have a global.  The PAK stays on the stack.
itemLookupGUID pak =
    let guidFinder = \x -> fromJust $ vUNIT_GUID x
        dat = readDATFiles pak "MEDIA/UNITS/ITEMS" guidFinder -- p is pak
    in (\idInt64 -> lkupDATFile dat idInt64, dat)

lookupPath pak =
    let ffp p = T.replace "\\" "/" p -- fix file path
    in \name -> lkupPAKFile (ffp name) pak >>= return . (runGetSuppress getDAT)

effectLookup pak =
    \effID -> lkupPAKFile "MEDIA/EFFECTSLIST.DAT" pak >>=
        return . (runGetSuppress getDAT) >>= case effID of
            EffectIndex i -> subNodeAt i
            EffectName n  -> searchNodeTreeWith (\node ->
                let mName = return node >>= vNAME
                in case mName of
                    Nothing   -> False
                    Just name -> n == T.unpack name)

-- Given a prefix path, makes a lookup table of pak files with NAME as lookup key
makeLookupByName path pak =
    let nameFinder = \x -> fromJust $ vNAME x >>= return . T.toUpper
        dat = readDATFiles pak path nameFinder
    in (\name -> lkupDATFile dat $ T.toUpper name)

affixLookup = makeLookupByName "MEDIA/AFFIXES/ITEMS"

skillLookup = makeLookupByName "MEDIA/SKILLS/"

monsterLookup = makeLookupByName "MEDIA/UNITS/MONSTERS/PETS/"

priceIsRightSearch :: LocationBytes -> (DATNode -> SlotID) -> [DATNode] -> DATNode
--priceIsRightSearch realPrice [] = who knows
priceIsRightSearch (LocationBytes {..}) pricer (guess:guesses) =
    let realPrice = lBytesSlotIndex
        i = fromIntegral . slotIDVal
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
        getName = vNAME
        slotsDatFiles = readDATFiles invenSlotFiles "MEDIA/INVENTORY" (fromJust . getName)
        allSlotTypesList = M.elems slotsDatFiles
        -- allSlotTypeList is a list of all Dat files for slots.  SlotDatFiles is a map of slot name
        -- to Dat file

        -- containers is a map of container ID to DATNode for the container
        containers = readDATFiles pak "MEDIA/INVENTORY/CONTAINERS" (fromJust . vContainerID)

        -- make a search function for finding the slot type with unique ID closest but no greater
        -- than the locBytes Word16
        getID = fromJust . vSlotID
        winningSlot locBytes = priceIsRightSearch locBytes getID allSlotTypesList

        -- Now to piece it all together
        locBytesToSlotCont (locBytes@LocationBytes {..}) =
            let cont = lkupDATFile containers $ ContainerID lBytesContainer
                slot = winningSlot locBytes
            in (slot, cont)

        -- Now create the reverse lookup: Given container and slot name, get container ID and slot ID
        slotNameToId :: String -> Maybe SlotID
        slotNameToId name = M.lookup (T.pack name) slotsDatFiles >>= vSlotID

        revContMap = M.fromList $ map (\(a,b) -> (fromJust $ getName b, a)) $ M.toList containers


        slotContToLocBytesContID slotName contName = (slotNameToId slotName, M.lookup (T.pack contName) revContMap)
    in (locBytesToSlotCont, slotContToLocBytesContID)


interp sortedPairs val
   | (not . isJust) topEndFind = snd $ last sortedPairs
   | (not . isJust) lowEndFind = snd $ head sortedPairs
   | otherwise = interpVal
   where
    topEndFind = L.find (\(x,y) -> x > val) sortedPairs
    lowEndFind = L.find (\(x,y) -> x <= val) $ reverse sortedPairs
    pointA = fromJust lowEndFind
    pointB = fromJust topEndFind
    yOf = snd
    xOf = fst
    rise = yOf pointB - (yOf pointA)
    run  = xOf pointB - (xOf pointA)
    interpVal = yOf pointA + (rise/run * (val - xOf pointA))

getPoints byPathFxn file =
    let Just dat = byPathFxn file
        pointNodes = datSubNodes dat
        mkPair pointNode = (fromJust $ vX pointNode, fromJust $ vY pointNode)
        points = map mkPair pointNodes
        sortFxn (x,y) (x',y') = compare x x'
        sortedPoints = L.sortBy sortFxn points
    in sortedPoints

graphLookup byPathFxn =
    (\graphFile value -> interp (getPoints byPathFxn graphFile) value)
    

triggerableLookup = makeLookupByName "MEDIA/TRIGGERABLES/"

statLookup = makeLookupByName "MEDIA/STATS/"

-- Recurses down from a Dat Node (usually an Item dat node) looking for a particular variable.
-- Looks deeper into each BASEFILE, if it exists, until it has to give up.
searchAncestryFor (env@Env{..}) findMeVar itemDat =
    let foundVar = return itemDat >>= findMeVar
        itemBase = return itemDat >>= vBASEFILE
    in case foundVar of
        Just var -> foundVar -- we found the data we want!
        Nothing ->
            itemBase >>= lkupPath >>= searchAncestryFor env findMeVar
