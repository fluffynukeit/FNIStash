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
   module Graphics.UI.Ji
) where

import FNIStash.Comm.Messages
import FNIStash.UI.Layout
import FNIStash.UI.Icon

import Graphics.UI.Ji
import Graphics.UI.Ji.Browser

import Control.Monad.Trans
import Control.Monad

import Debug.Trace
import Data.Maybe

frontend :: MonadJi m => Chan (Message BMessage) -> m ()
frontend messages = do
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

