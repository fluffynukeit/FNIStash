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
    launchBackend
) where

import FNIStash.Logic.Initialize
import FNIStash.File.Location
import FNIStash.Comm.Messages
import FNIStash.File.Crypto
import FNIStash.File.SharedStash

import Filesystem.Path
import Filesystem.Path.CurrentOS
import Control.Concurrent
import Control.Monad.Trans
import Control.Monad

import Debug.Trace

-- These are paths to test assets, so I don't mess up my real ones.  Delete later.
testDir = "C:\\Users\\Dan\\Desktop\\FNI Testing"
sharedStashCrypted = testDir </> "sharedstash_v2.bin"
textOutputPath = testDir </> "sharedStashTxt.txt"


-- Start up the backend and do some path setup
launchBackend messages = do
    -- Ensure we have an app path for both backend and GUI to access
    appRoot <- ensureAppRoot
    guiRoot <- ensureHtml appRoot
    forkIO $ backend messages appRoot guiRoot
    return (appRoot, guiRoot)


-- The real meat of the program
backend messages appRoot guiRoot = do
    env <- initialize messages appRoot guiRoot

     -- Descramble the scrambled shared stash file.  Just reads the test file for now. Needs to
    -- eventually read the file defined by cfg
    ssData <- readCryptoFile (encodeString sharedStashCrypted) >>= return . fileGameData

    let sharedStashResult = parseSharedStash env ssData
    case sharedStashResult of
        Left error -> writeBMessage messages $ Error error
        Right sharedStash -> do
            dumpItemLocs messages sharedStash
            msgList <- liftIO $ onlyFMessages messages
            handleMessages messages sharedStash msgList



dumpItemLocs messages sharedStash = mapM_ dumpItem sharedStash where
    dumpItem i = writeBMessage messages $ case i of
        Left itemError -> Error itemError
        Right item -> LocationContents (itemLocation item) $ Just item


handleMessages m sharedStash (msg:rest) = do
    let (newStash, updates) = case msg of
            Move from to -> moveContents from to sharedStash
    forM updates $ \(loc, contents) -> writeBMessage m $ LocationContents loc contents
    handleMessages m newStash rest


