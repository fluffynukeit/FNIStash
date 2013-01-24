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
import FNIStash.File.DAT

import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import Data.Binary.Get
import Control.Monad
import Data.Monoid
import Data.Word

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
    size <- getWord32le
    getLazyByteString $ fromIntegral size

getSharedStash :: Get SharedStash
getSharedStash = do
    parts <- getSharedStashPartitions
    return $ map (runGetWithFail "Could not parse item!" getItem) parts

textSharedStash :: (Word32 -> Maybe DATNode) -> SharedStash -> T.Text
textSharedStash effSearch s =
    foldl (\a b -> a <> textItemResult effSearch b) T.empty s

textItemResult effSearch (Left error) = T.unlines ["", error, ""]
textItemResult effSearch (Right item) = textItem effSearch item
