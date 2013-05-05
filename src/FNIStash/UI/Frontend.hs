-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.UI.Frontend
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

module FNIStash.UI.Frontend (
   frontend,
   module Graphics.UI.Threepenny
) where

import FNIStash.Comm.Messages
import FNIStash.UI.Layout
import FNIStash.UI.Icon
import FNIStash.UI.Effects

import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Browser

import Control.Monad.Trans
import Control.Monad

import Debug.Trace
import Data.Maybe

frontend :: Messages -> TP ()
frontend messages = do
    forkTP handleEvents -- start the event handler.
    setTitle "FNIStash"
    body <- getBody
    msgWindow <- controls messages body
    msgList <- liftIO $ onlyBMessages messages
    forM_ msgList $ \x -> do
        case x of
            Initializing msg -> return msgWindow #= (msg ++ "...") # unit
            Info msg -> return msgWindow #= msg # unit
            Error msg -> return msgWindow #= ("Error: " ++ msg) # unit
            Initialized -> stash messages #+ body # unit
            LocationContents loc mItem -> withCell loc $ updateItem mItem
            Saved -> return msgWindow #= "Data saved!" # unit
            Registered locList -> forM_ locList (\loc -> withCell loc $ flashElement 500)
            _ -> return ()
