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
import FNIStash.File.SharedStash

import Graphics.UI.Ji
import Graphics.UI.Ji.Browser

import Control.Monad.Trans
import Control.Monad

frontend :: MonadJi m => Chan (Message BMessage) -> m ()
frontend messages = do
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
