-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.Backend
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
{-# LANGUAGE ViewPatterns #-}

module FNIStash.Logic.Backend (
    ensurePaths,
    backend
) where

import FNIStash.Logic.Initialize
import FNIStash.File.Location
import FNIStash.Comm.Messages
import FNIStash.File.Crypto
import FNIStash.File.SharedStash
import FNIStash.Logic.Items
import FNIStash.Logic.DB

import Filesystem.Path
import Filesystem.Path.CurrentOS
import Control.Monad.Trans
import Control.Monad
import Control.Exception
import Data.Either

import Debug.Trace

-- These are paths to test assets, so I don't mess up my real ones.  Delete later.
testDir = "C:\\Users\\Dan\\Desktop\\FNI Testing"
sharedStashCrypted = testDir </> "sharedstash_v2.bin"
textOutputPath = testDir </> "sharedStashTxt.txt"
savePath = testDir </> "testSave.bin"

-- Gets/makes the necessary application paths
ensurePaths = do
    -- Ensure we have an app path for both backend and GUI to access
    appRoot <- ensureAppRoot
    guiRoot <- ensureHtml appRoot
    return (appRoot, guiRoot)

sendErrIO :: Messages -> IOException -> IO ()
sendErrIO msg exc = writeBMessage msg $ Notice $ Error ("DB: " ++ show exc)
sendErrDB msg exc = writeBMessage msg $ Notice $ Error ("DB: " ++ show exc)

-- The real meat of the program
backend msg appRoot guiRoot = handle (sendErrIO msg) $ handleDB (sendErrDB msg) $ do
    env <- initialize msg appRoot guiRoot

     -- Descramble the scrambled shared stash file.  Just reads the test file for now. Needs to
    -- eventually read the file defined by cfg
    cryptoFile <- readCryptoFile (encodeString sharedStashCrypted)
    let ssData = fileGameData cryptoFile

    let sharedStashResult = parseSharedStash env ssData
    case sharedStashResult of
        Left error -> writeBMessage msg $ Notice $ Error error
        Right sharedStash -> do
            dumpItemLocs msg sharedStash
            dumpRegistrations env msg sharedStash
            msgList <- liftIO $ onlyFMessages msg
            handleMessages env msg cryptoFile sharedStash msgList

dumpItemLocs messages sharedStash = mapM_ dumpItem sharedStash where
    dumpItem i = writeBMessage messages $ case i of
        Left itemError -> Notice $ Error itemError
        Right item -> LocationContents (itemLocation item) $ Just item

dumpRegistrations env messages sharedStash = do
    writeBMessage messages $ Notice $ Info "Registering items..."
    registeredItems <- registerStash env sharedStash
    let locations = map itemLocation registeredItems
    writeBMessage messages $ Registered locations
    writeBMessage messages $ Notice $ Info $ "Newly registered items: " ++ (show $ length locations)

-- Tries to register all non-registered items into the DB.  Retuns list of newly
-- registered items.
registerStash env sharedStash =
    let parsedItems = rights sharedStash
    in register env parsedItems


handleMessages env m cryptoFile sharedStash (msg:rest) = do
    (newStash, updates) <- case msg of
            Move from to -> moveContents from to sharedStash
            Save -> do
                a <- saveItems env cryptoFile sharedStash (encodeString savePath)
                writeBMessage m $ Notice $ Saved (encodeString savePath)
                return a
            Search keywordsString -> do
                writeBMessage m $ Notice $ Info "Searching..."
                matchStatuses <- locIDsKeywordStatus env $ words keywordsString
                writeBMessage m $ Visibility matchStatuses
                return (sharedStash, [])
    forM updates $ \(loc, contents) -> writeBMessage m $ LocationContents loc contents
    handleMessages env m cryptoFile newStash rest

