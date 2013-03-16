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

import Control.Monad.Trans
import Control.Monad

import Graphics.UI.Ji
import Graphics.UI.Ji.JQuery
import Control.Concurrent

main = do
    messages <- newChan
    backend messages
--    serve Config
--        { jiPort = 10001
--        , jiRun = runJi
--        , jiWorker = worker messages
--        , jiInitHTML = "GUI.html"
--        , jiStatic = "C:\\Users\\Dan\\My Code\\FNIStash\\wwwroot"
--        }

-- Start up the backend
backend messages = do
    writeChan messages (Message Initializing)
    result <- initialize
    writeChan messages result


worker :: MonadJi m => Chan (Message Response) -> m ()
worker messages = do
    setTitle "FNIStash"
    body <- getBody
    element <- newElement "div"
    appendTo body element
    msgList <- liftIO $ getChanContents messages
    forM_ msgList $ \(Message x) -> do
        flip setText element $ case x of
            Initializing -> "Initializing..."
            Initialized -> "Finally initialized!"
            Error -> "Something went wrong..."

    
