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
import Control.Exception
import Filesystem
import Filesystem.Path.CurrentOS
import System.Environment
import Data.Maybe

version = "r1.2"

main = do

    args <- getArgs
    paths <- ensurePaths $ listToMaybe args
    mvar <- newEmptyMVar
    
    serve Config
        { tpPort = 10001
        , tpRun = runTP
        , tpWorker = launchAll paths mvar
        , tpInitHTML = Just "GUI.html"
        , tpStatic = encodeString (guiRoot paths)
        }

launchAll paths mvar = do
    messages <- liftIO newMessages
    liftIO $ forkIO $ backend messages paths mvar
    frontend version messages

