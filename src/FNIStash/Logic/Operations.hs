-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Items
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

{-# LANGUAGE RecordWildCards #-}

module FNIStash.Logic.Operations (
    moveContents,
    saveItems
) where

import FNIStash.File.SharedStash
import FNIStash.File.Crypto
import Data.List
import Data.Binary.Put
import qualified Data.ByteString.Lazy as BS



moveTo loc (Item {..}) =
    Item iName iRandomID iIdentified loc iLevel iQuantity iNumSockets iGems iGemsAsItems iInnateDefs iEffectsRaw
         iEffects iEnchantments iTriggerables iPartition iBase


-- Tries to add an item to the shared stash.  If an item at the desired location already
-- exists, that item is swapped.  Return is both the updated shared stash as well as
-- a list of location contents' updates made.
updateSharedStash :: Location -> Maybe Item -> SharedStash -> (SharedStash, [(Location, Maybe Item)])
updateSharedStash loc Nothing stash =
    let match = findMatchingItem loc stash
    in case match of
        Nothing -> (stash, []) -- no location updates made
        Just item -> (delete item stash, [(loc, Nothing)]) -- loc now has Nothing

updateSharedStash newLoc (Just item) stash =
    let matchDestination = findMatchingItem newLoc stash
        matchSource = findMatchingItem oldLoc stash
        oldLoc = iLocation item
        -- TODO handle cases where dest and src are the same

    in case matchDestination of
        Nothing -> -- the destination is empty.  No swapping.
            case matchSource of
                Nothing -> (Right (moveTo newLoc item) : stash,
                            [(newLoc, Just item)]) -- and the source location is already empty.  Insert new guy.
                Just oldLocItem -> (Right (moveTo newLoc item) : delete oldLocItem stash,
                                    [(newLoc, Just item), (oldLoc, Nothing)])  -- remove and update location
        Just rDestItem@(Right destItem) ->
            case matchSource of
                Just k -> ( Right (moveTo newLoc item) :
                            Right (moveTo oldLoc destItem) :
                            (delete rDestItem $ delete k stash),
                            [(newLoc, Just item), (oldLoc, Just destItem)])
                Nothing ->( Right (moveTo newLoc item) : delete rDestItem stash,
                            [(newLoc, Just item)])

findMatchingItem loc = find $ \x -> (isRight x) && loc == iLocation (fromRight x)


isRight (Right _) = True
isRight _ = False

fromRight (Right x) = x

-- START ITEM OPERATIONS

moveContents fromLoc toLoc stash = do
    let foundItem = findMatchingItem fromLoc stash
    case foundItem of
        Nothing -> return $ updateSharedStash fromLoc Nothing stash -- don't do anything
        Just (Right item) -> return $ updateSharedStash toLoc (Just item) stash

sharedStashToBS env ss = runPut (putSharedStash env ss)

saveItems env c ss filePath = do
    let i = sharedStashToBS env ss
        newSaveFile = CryptoFile (fileVersion c) (fileDummy c) (0) (i) (0)
    writeCryptoFile filePath newSaveFile
    return (ss, [])
