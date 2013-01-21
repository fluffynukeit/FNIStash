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
    getSharedStash,
    textSharedStash
) where

import FNIStash.Logic.Item
import FNIStash.File.Item
import FNIStash.File.General

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.Binary.Get
import Control.Monad
import Data.Monoid

-- NOTE: type and to text functions should be in Logic for consistency, similar to Item
type SharedStash = [Either T.Text Item]

getSharedStashPartitions :: Get [BS.ByteString]
getSharedStashPartitions = do
    numItems <- getWord32le
    bsList <- replicateM (fromIntegral numItems) getIndividualPartition
    if fromIntegral numItems /= length bsList then
        fail $ "Number of Shared Stash items " ++ show numItems ++
            "does not match number parsed " ++ (show . length) bsList
        else
            return bsList

getIndividualPartition :: Get BS.ByteString
getIndividualPartition = do
    size <- getWord16le
    skip 3 -- 3 bytes are used for nothing, always 0
    getLazyByteString (fromIntegral size - 1)

getSharedStash :: Get SharedStash
getSharedStash = do
    parts <- getSharedStashPartitions
    return $ map (runGetWithFail "Could not parse item!" getItem) parts

textSharedStash :: SharedStash -> T.Text
textSharedStash s =
    foldl (\a b -> a <> textItemResult b) T.empty s

textItemResult (Left error) = T.unlines ["", error, ""]
textItemResult (Right item) = textItem item
