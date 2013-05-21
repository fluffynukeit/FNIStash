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
            Initialized -> stash messages #+ body # unit
            LocationContents locItemsList -> withLocVals locItemsList updateItem
            Notice notice -> noticeDisplay notice # addTo msgWindow >> scrollToBottom msgWindow
            Registered locList -> withLocVals (zip locList locList) (\e _ _ -> flashElement 500 e)
            Visibility idStatusList -> withLocVals idStatusList $ \e v _ -> setVis v e # unit
                                 
noticeDisplay notice = do
    msgDisp <- new #. "notice"
    case notice of
        Error msg   -> new #. "error" #= msg #+ msgDisp # unit
        Info msg    -> new #. "info" #= msg #+ msgDisp # unit
        Saved path  -> new #. "saved" #= "Shared stash saved to " ++ path #+ msgDisp # unit
    return msgDisp



