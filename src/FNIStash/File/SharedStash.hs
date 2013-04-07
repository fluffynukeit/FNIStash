-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.File.SharedStash
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

module FNIStash.File.SharedStash (
    parseSharedStash,
    showSharedStash,
    SharedStash(..),
    moveContents,
    module FNIStash.File.Item
) where

import FNIStash.File.Item
import FNIStash.File.General
import FNIStash.Logic.Env

import qualified Data.ByteString as BS
import Data.Binary.Strict.Get
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Word
import Data.List

-- NOTE: type and to text functions should be in Logic for consistency, similar to Item
type SharedStash = [Either String Item]

parseSharedStash env ssData = runGetWithFail "Can't read shared stash file!" (getSharedStash env) (toStrict ssData)

getSharedStashPartitions :: Get [BS.ByteString]
getSharedStashPartitions = do
    numItems <- getWord32le
    bsList <- replicateM (fromIntegral numItems) getIndividualPartition
    if fromIntegral numItems /= length bsList then
        fail $ "Number of Shared Stash items " ++ show numItems ++
            "does not match number parsed " ++ (show . length) bsList
        else
            return bsList

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
        oldLoc = itemLocation item
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

findMatchingItem loc = find $ \x -> (isRight x) && loc == itemLocation (fromRight x)

moveContents fromLoc toLoc stash =
    let foundItem = findMatchingItem fromLoc stash
    in case foundItem of
        Nothing -> updateSharedStash fromLoc Nothing stash -- don't do anything
        Just (Right item) -> updateSharedStash toLoc (Just item) stash

isRight (Right _) = True
isRight _ = False

fromRight (Right x) = x

getIndividualPartition :: Get BS.ByteString
getIndividualPartition = do
    size <- getWord32le
    getByteString $ fromIntegral size

getSharedStash :: Env -> Get SharedStash
getSharedStash env = do
    parts <- getSharedStashPartitions
    return $ map (\bs -> runGetWithFail "Could not parse item!" (getItem env bs) bs) parts

showSharedStash :: SharedStash -> String
showSharedStash s = foldl (\a b -> a <> showItemResult b) "" s

showItemResult (Left error) = unlines ["", error, ""]
showItemResult (Right item) = showItem item
