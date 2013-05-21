-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.File.Location
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

module FNIStash.File.Location where

import FNIStash.Logic.Env
import FNIStash.File.Variables
import FNIStash.File.DAT

import qualified Data.ByteString as BS
import Data.Binary.Strict.Get
import Data.Binary.Put
import Data.Word

data Location = Location
    { locContainer :: String
    , locSlot :: String
    , locIndex :: Int
    }
    | Inserted
    deriving (Eq, Ord, Show)


getLocation env = do
    locBytes <- getWord16le
    containerID <- getWord16le
    let l = lkupLocNodes env
        (slotType, Just container) = l locBytes containerID
        -- get the Container name
        Just containerName = lkupVar vNAME container >>= stringVar
        Just slotName = lkupVar vNAME slotType >>= stringVar
        Just slotID = lkupVar vUNIQUEID slotType >>= word32Var
        index = fromIntegral locBytes - slotID
    return $ if locBytes == 0xFFFF && containerID == 0xFFFF
             then Inserted
             else Location containerName slotName (fromIntegral index)

putLocation :: Env -> Location -> Put
putLocation env loc = do
    let l = lkupLocIDs env
        (Just slotID, Just contID) = l (locSlot loc) (locContainer loc)
    putWord16le (slotID + (fromIntegral $ locIndex loc))
    putWord16le contID

