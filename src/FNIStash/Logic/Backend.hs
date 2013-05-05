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

-- The real meat of the program
backend messages appRoot guiRoot = handleDBError $ do
    env <- initialize messages appRoot guiRoot

     -- Descramble the scrambled shared stash file.  Just reads the test file for now. Needs to
    -- eventually read the file defined by cfg
    cryptoFile <- readCryptoFile (encodeString sharedStashCrypted)
    let ssData = fileGameData cryptoFile

    let sharedStashResult = parseSharedStash env ssData
    case sharedStashResult of
        Left error -> writeBMessage messages $ Error error
        Right sharedStash -> do
            dumpItemLocs messages sharedStash
            dumpRegistrations env messages sharedStash
            msgList <- liftIO $ onlyFMessages messages
            handleMessages env messages cryptoFile sharedStash msgList

dumpItemLocs messages sharedStash = mapM_ dumpItem sharedStash where
    dumpItem i = writeBMessage messages $ case i of
        Left itemError -> Error itemError
        Right item -> LocationContents (itemLocation item) $ Just item

dumpRegistrations env messages sharedStash = do
    registeredItems <- registerStash env sharedStash
    let locations = map itemLocation registeredItems
    writeBMessage messages $ Registered locations
    writeBMessage messages $ Info $ "Newly registered items: " ++ (show $ length locations)

-- Tries to register all non-registered items into the DB.  Retuns list of newly
-- registered items.
registerStash env sharedStash =
    let parsedItems = rights sharedStash
    in filterM (register env) $ trace ("Parsed items: " ++ (show $ length parsedItems)) parsedItems


handleMessages env m cryptoFile sharedStash (msg:rest) = do
    (newStash, updates) <- case msg of
            Move from to -> moveContents from to sharedStash
            Save -> do
                a <- saveItems env cryptoFile sharedStash (encodeString savePath)
                writeBMessage m Saved
                return a
    forM updates $ \(loc, contents) -> writeBMessage m $ LocationContents loc contents
    handleMessages env m cryptoFile newStash rest
