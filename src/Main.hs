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


import FNIStash.Logic.Backend
import FNIStash.UI.Frontend
import FNIStash.Comm.Messages

import Control.Concurrent
import Control.Monad.Trans
import Filesystem
import Filesystem.Path.CurrentOS

main = do
    setWorkingDirectory "C:\\Users\\Dan\\My Code\\FNIStash" -- only for testing
    (appRoot, guiRoot) <- ensurePaths
    
    serve Config
        { tpPort = 10001
        , tpRun = runTP
        , tpWorker = launchAll appRoot guiRoot
        , tpInitHTML = Just "GUI.html"
        , tpStatic = encodeString guiRoot
        }

launchAll appRoot guiRoot = do
    messages <- liftIO newMessages
    liftIO $ forkIO $ backend messages appRoot guiRoot
    frontend messages
