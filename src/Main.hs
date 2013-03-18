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


import FNIStash.Logic.Initialize
import FNIStash.Comm.Messages

import qualified Data.Text as T

import Filesystem
import Filesystem.Path
import Filesystem.Path.CurrentOS

import Control.Monad.Trans
import Control.Monad

import Graphics.UI.Ji
import Graphics.UI.Ji.JQuery
import Graphics.UI.Ji.Elements
import Graphics.UI.Ji.DOM
import Control.Concurrent

main = do
    setWorkingDirectory "C:\\Users\\Dan\\My Code\\FNIStash" -- only for testing
    -- Ensure we have an app path for both backend and GUI to access
    appRoot <- ensureAppRoot
    guiRoot <- ensureHtml appRoot
    messages <- newChan
    forkIO $ backend messages appRoot guiRoot
    serve Config
        { jiPort = 10001
        , jiRun = runJi
        , jiWorker = worker messages
        , jiInitHTML = "GUI.html"
        , jiStatic = encodeString guiRoot
        }

-- Start up the backend
backend messages appRoot guiRoot = do
    writeChan messages (Message Initializing)
    result <- initialize appRoot guiRoot
    writeChan messages result

worker :: MonadJi m => Chan (Message Response) -> m ()
worker messages = do
    setTitle "FNIStash"
    body <- getBody
    div <- new #. "div" #+ body
    msgList <- liftIO $ getChanContents messages
    forM_ msgList $ \(Message x) -> do
        case x of
            Initializing -> setText "Initializing..." div
            Error -> setText "Something went wrong..." div
            Initialized -> do
                setText "Finally initialized!" div
                newImg # set "src" "static/GUIAssets/bell.png" # set "alt" "Bell pict" #+ body

    
