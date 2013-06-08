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
    putSharedStash,
    SharedStash(..),
    module FNIStash.File.Item
) where

import FNIStash.File.Item
import FNIStash.File.General
import FNIStash.Logic.Env

import qualified Data.ByteString as BS
import Data.Binary.Strict.Get
import Data.Binary.Put
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Word


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

putSharedStash :: Env -> SharedStash -> Put
putSharedStash env ss = do
    putWord32le $ fromIntegral $ length ss
    forM_ ss $ \elem -> do
        case elem of
            Right item  -> putItem env item
            _           -> return ()

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
showItemResult (Right item) = show item
