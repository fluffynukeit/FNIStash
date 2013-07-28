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

{-# LANGUAGE RecordWildCards #-}

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
import System.Random
import Data.Time.LocalTime

import Debug.Trace
import Data.Maybe

frontend :: Messages -> TP ()
frontend messages = do
    forkTP handleEvents -- start the event handler.
    setTitle "FNIStash"
    body <- getBody
    (overlay, overlayMsg) <- overlay
    return overlay #+ body
    underlay <- new ## "underlay" #+ body
    frame <- new ## "frame" #+ underlay
    msgWindow <- controls messages frame
    msgList <- liftIO $ onlyBMessages messages
    (stash, updateTxt, grailTxt) <- stash messages
    forM_ msgList $ \x -> do
        case x of
            Initializing AssetsComplete -> return stash #+ frame # unit
            Initializing Complete -> do
                assignRandomBackground underlay
                crossFade overlay underlay 350
            Initializing (ArchiveData summs) -> populateArchiveTable messages summs
            Initializing (ReportData iReport)-> updateReport grailTxt iReport 
                
            Initializing x                -> handleInit x overlayMsg
                
            LocationContents locItemsList -> withLocVals locItemsList updateCell
            Notice notice                 -> noticeDisplay notice # addTo msgWindow >> scrollToBottom msgWindow
            Visibility idStatusList       -> setVisOfMatches idStatusList
            ResponseItem elem mitem       -> maybe (return ()) (flip makePopUp elem) mitem
                

matchToLocBool (ItemMatch _ _ Nothing) = Nothing
matchToLocBool (ItemMatch id flag (Just loc)) = Just (loc, flag)

setVisOfMatches matchList = do
    -- first set visibility of any items still in stash
    withLocVals (catMaybes $ map (matchToLocBool) matchList) $ \e _ v _ -> setFaded (not v) e # unit
    -- then set visibility of archive table rows
    archRows <- getElementsById $ map (locToId . Archive . matchDbID) matchList
    let rowBool = zip archRows $ map matchFlag matchList
    forM_ rowBool $ \(e, d) -> setDisp d e # unit

noticeDisplay notice = do
    zonedTime <- liftIO getZonedTime
    let localTime = localTimeOfDay $ zonedTimeToLocalTime zonedTime
        mkMsg = (++) (pp0 (todHour localTime) ++ ":" ++ pp0 (todMin localTime) ++ ": ")
        pp0 a = let t = show a in if length t >= 2 then t else "0"++t
    msgDisp <- new #. "notice"
    case notice of
        Error msg   -> new #. "error" #= mkMsg msg #+ msgDisp # unit
        Info msg    -> new #. "info" #= mkMsg msg #+ msgDisp # unit
        Saved path  -> new #. "saved" #= mkMsg ("Shared stash saved to " ++ path) #+ msgDisp # unit
    return msgDisp

assignRandomBackground el = do
    let suffs = ["","2","3","4","5","6"]; -- screen 7 doesn't look good with it
    roll <- liftIO $ getStdRandom (randomR (0,5)) :: TP (Int)
    let url = "url('/static/GUIAssets/SCREENS" ++ (suffs !! roll) ++ ".png')"
    return el # setStyle [("backgroundImage", url)] # unit


initMsg msg overlayMsg = return overlayMsg #= msg # unit

handleInit CfgStart = initMsg "Reading configuration file..."
handleInit DBStart  = initMsg "Instantiating database..."
handleInit AssetsStart = initMsg "Extracting assets for first time startup.  Please wait..."
handleInit EnvStart = initMsg "Building lookup environment..."
handleInit RegisterStart = initMsg "Registering new items..."
handleInit Complete = initMsg "Startup complete."
handleInit (InitError s) = initMsg s
handleInit ArchiveDataStart = initMsg "Retrieving archived items..."
handleInit ReportStart = initMsg "Building item report..."
handleInit _        = initMsg "Unknown initialization event!"
