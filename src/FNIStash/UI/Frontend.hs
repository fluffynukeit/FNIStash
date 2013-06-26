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

import Control.Monad.Trans
import Control.Monad
import System.Random

import Debug.Trace
import Data.Maybe

--frontend :: Messages -> Window -> Dom (IO ())
frontend messages w = do
    
    set title "FNIStash" (return w)
    body <- getBody w
    
    (overlay, overlayMsg) <- withWindow w $ overlay
    element body #+ [element overlay]
    underlay <- withWindow w $ new # set (attr "id") "underlay"
    element body #+ [element underlay]
    frame <- withWindow w $ new # set (attr "id") "frame"
    element underlay #+ [element frame]
    msgWindow <- withWindow w $ controls messages (element frame)
    msgList <- liftIO $ onlyBMessages messages
    
    forM_ msgList $ \x -> do
        case x of
            Initializing AssetsComplete -> void $ withWindow w $ stash messages #+ [element frame]
            Initializing Complete -> do
                assignRandomBackground underlay
                crossFade overlay underlay 350
                
            Initializing x -> void $ handleInit x overlayMsg
                
            LocationContents locItemsList -> withLocVals w locItemsList (updateItem)
            Notice notice -> void $ withWindow w $ noticeDisplay notice # appendTo msgWindow >> return (scrollToBottom msgWindow)
            Visibility idStatusList -> withLocVals w idStatusList $ \e v _ -> setVis v (element e)
                                 
noticeDisplay notice = do
    msgDisp <- new #. "notice"
    let k = case notice of
            Error msg   -> new #. "error" # set text msg
            Info msg    -> new #. "info" # set text msg
            Saved path  -> new #. "saved" # set text ("Shared stash saved to " ++ path)
    element msgDisp #+ [k]
    return msgDisp

assignRandomBackground el = do
    let suffs = ["","2","3","4","5","6"]; -- screen 7 doesn't look good with it
    roll <- liftIO $ getStdRandom (randomR (0,5))
    let url = "url('/static/GUIAssets/SCREENS" ++ (suffs !! roll) ++ ".png')"
    return el # set style [("backgroundImage", url)] 


initMsg msg overlayMsg = return overlayMsg # set text msg

handleInit CfgStart = initMsg "Reading configuration file..."
handleInit DBStart  = initMsg "Instantiating database..."
handleInit AssetsStart = initMsg "Extracting assets for first time startup.  Please wait..."
handleInit EnvStart = initMsg "Building lookup environment..."
handleInit RegisterStart = initMsg "Registering new items..."
handleInit Complete = initMsg "Startup complete."
handleInit (InitError s) = initMsg s
handleInit _        = initMsg "Unknown initialization event!"
