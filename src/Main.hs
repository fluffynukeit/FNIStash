-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where



import FNIStash.Comm.Messages
import FNIStash.Logic.Backend
import FNIStash.File.SharedStash

import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS

import Control.Monad.Trans
import Control.Monad

import Graphics.UI.Ji
import Graphics.UI.Ji.Browser

main = do
    setWorkingDirectory "C:\\Users\\Dan\\My Code\\FNIStash" -- only for testing
    messages <- newChan
    (appRoot, guiRoot) <- launchBackend messages
    serve Config
        { jiPort = 10001
        , jiRun = runJi
        , jiWorker = worker messages
        , jiInitHTML = "GUI.html"
        , jiStatic = encodeString guiRoot
        }

worker :: MonadJi m => Chan (Message BMessage) -> m ()
worker messages = do
    setTitle "FNIStash"
    body <- getBody
    div <- new #. "div" #+ body
    msgList <- liftIO $ getChanContents messages
    forM_ msgList $ \(Message x) -> do
        case x of
            Initializing msg -> setText (msg ++ "...") div
            Error msg -> setText ("Error: " ++ msg) div
            Initialized -> do
                setText "Finally initialized!" div
            LocationContents loc item -> do
                newImg # set "src" (maybe "defaultIcon.png" iconPath item)
                       # set "alt" (maybe "Unknown name" iconPath item)
                       #+ body
iconPath i = "static/GUIAssets/" ++ (itemIcon i) ++ ".png"
