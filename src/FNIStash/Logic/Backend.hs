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
{-# LANGUAGE RecordWildCards #-}

module FNIStash.Logic.Backend (
    ensurePaths,
    backend
) where

import FNIStash.Logic.Initialize
import FNIStash.Comm.Messages
import FNIStash.File.Crypto
import FNIStash.File.SharedStash
import FNIStash.Logic.Operations
import FNIStash.Logic.DB

import Filesystem.Path
import Filesystem.Path.CurrentOS
import Control.Monad.Trans
import Control.Monad
import Control.Exception
import Data.Either
import Data.List.Split
import qualified Data.List as L

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
        Left error -> writeBMessage msg $ Initializing $ InitError $ "Error reading shared stash: " ++ error
        Right sharedStash -> do
            writeBMessage msg $ Initializing RegisterStart
            dumpRegistrations env msg sharedStash
            dumpItemLocs msg env
            writeBMessage msg $ Initializing ArchiveDataStart
            dumpArchive env msg
            writeBMessage msg $ Initializing Complete
            msgList <- liftIO $ onlyFMessages msg
            handleMessages env msg cryptoFile msgList

dumpItemLocs messages env = do
    eitherConts <- allLocationContents env
    let itemErrors = lefts eitherConts
        goodLocItems  = rights eitherConts
        locMsg = LocationContents goodLocItems
    writeBMessage messages locMsg
    forM_ itemErrors $ \err -> writeBMessage messages $
        Notice $ Error $ err
    when (length itemErrors > 0) $ writeBMessage messages $
         Notice $ Error $ "Number items failed: " ++ (show.length) itemErrors


dumpArchive env msg = do
    allItems <- allItemSummaries env
    writeBMessage msg $ Initializing $ ArchiveData allItems

dumpRegistrations env messages sharedStash = do

    registeredItems <- registerStash env sharedStash
    let locations = map iLocation registeredItems
    writeBMessage messages $ Notice $ Info $ "Newly registered items: " ++ (show $ length locations)

-- Tries to register all non-registered items into the DB.  Retuns list of newly
-- registered items.
registerStash env sharedStash =
    let parsedItems = rights sharedStash
    in register env parsedItems

-- This is the main backend event queue
handleMessages env m cryptoFile (msg:rest) = do
    outMessages <- case msg of

        -- Move an item from one location to another
        Move fromToList -> do
            changeResults <- forM fromToList $ \(from, to) -> locationChange env from to
            let errorStrings = lefts changeResults
                contentUpdates = L.concat $ rights changeResults
                makeNotice erro = Notice . Error $ "Move error " ++ erro
            return $ (map makeNotice errorStrings) ++ [LocationContents contentUpdates]

        -- Save all "Stashed" items to disk
        Save -> do
            --a <- saveItems env cryptoFile sharedStash (encodeString savePath)
            return [Notice $ Saved (encodeString savePath)]

        -- Find items matching keywords
        Search keywordsString -> do
            matchStatuses <- keywordStatus env keywordsString
            return $ case matchStatuses of
                Right visibilityUpdates -> [Visibility visibilityUpdates]
                Left  queryError        -> [Notice . Error $ "Query parse error " ++ queryError]

        -- Make a request for item data, associated with a particular element
        RequestItem elem loc -> do
            dbResult <- getItemFromDb env loc
            return $ case dbResult of
                Left requestErr -> [Notice . Error $ requestErr]
                Right mitem     -> [ResponseItem elem mitem]

    -- send GUI updates
    forM_ outMessages $ \msg -> writeBMessage m msg

    -- and then process next message
    handleMessages env m cryptoFile rest

