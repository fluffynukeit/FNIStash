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

import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Browser

import Control.Monad.Trans
import Control.Monad

import Debug.Trace
import Data.Maybe

frontend :: Chan (Message BMessage) -> Ji ()
frontend messages = do
    forkJi handleEvents -- start the event handler.
    setTitle "FNIStash"
    body <- getBody
    div <- new #+ body
    msgList <- liftIO $ getChanContents messages
    forM_ msgList $ \(Message x) -> do
        case x of
            Initializing msg -> return div #= (msg ++ "...") # unit
            Error msg -> return div #= ("Error: " ++ msg) # unit
            Initialized -> stash x #+ body # unit
            LocationContents loc mItem -> do
                case mItem of
                    Just item -> newItemIcon item # insertAt loc
                    Nothing -> return ()


