-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.UI.Icon
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
{-# LANGUAGE ViewPatterns #-}

module FNIStash.UI.Icon
where

import FNIStash.File.SharedStash
import FNIStash.UI.Effects
import FNIStash.Comm.Messages

import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Browser

import Control.Monad
import Control.Monad.Trans
import Data.Maybe

import Debug.Trace

newItemIcon (item@Item {..}) = do
    container <- new # setStyle [("position", "relative")] # allowDrag
    
    i <- newIcon (iBaseIcon iBase)
        #. "item"
    return i #+ container

    -- create full and empty socket icons
    let numFullSockets = length iGems
    forM_ [1..numFullSockets] $ \ind ->
        newIcon "socket_full" #. ("socket" ++ show ind) #+ container

    forM_ [numFullSockets+1..iNumSockets] $ \ind ->
        newIcon "socket_empty" #. ("socket" ++ show ind) #+ container

    case iQuantityRaw of
        1   -> return ()
        qty -> new #. "qty" #= show qty #+ container # unit
    
    killPopUp

    onHover container $ \_ -> killPopUp >> makePopUp item container
    onBlur container $ \_ -> killPopUp
    onDragStart container $ \_ -> killPopUp
    
    return container

moveScript = "var pop = document.getElementById(\"itempopup\"); pop.style.visibility = \"inherit\";\
             \if (event.clientX < document.body.clientWidth/2) \
             \{pop.style.left=(event.clientX+50)+\"px\";}\
             \else {pop.style.right=(document.body.clientWidth - event.clientX+65)+\"px\";}\
             \pop.style.top=\
                \(event.clientY - (event.clientY*1.0/document.body.clientHeight)*pop.offsetHeight)+\"px\""


makePopUp item container = makePopUpBox item >> setAttr "onmousemove" moveScript container # unit

makePopUpBox (Item{..}) = do
    body <- getBody
    container <- new #. "itempopup" ## "itempopup"
    titleArea <- new #. "poptitlearea"
    new #. "poplevel" #= show iLevel #+ titleArea
    newIcon (iBaseIcon iBase) #. "popicon" #+ titleArea

    let qualityClass = case (uQuality . iBaseUnitType $ iBase) of
            NormalQ     -> "popnormal poptitle"
            MagicQ      -> "popmagic poptitle"
            UniqueQ     -> "popunique poptitle"
            LegendaryQ  -> "poplegendary poptitle"
            _           -> "popnormal poptitle"
    
    new #. qualityClass #= show iName #+ titleArea

    dataArea <- new #. "popdataarea"
    forM_ (iBaseInnates iBase) $ \inn ->
        new #. "popinnate poptext" #= show inn #+ dataArea

    forM_ (iInnateDefs) $ \pair -> makeInnateDef pair dataArea

    case iQuantity of
        Nothing -> return ()
        Just des-> new #. "popinnate poptext" #= show des #+ dataArea # unit

    -- Sockets
    forM_ iGems $ \gem -> makeFullSocket gem dataArea

    forM_ [1..(iNumSockets - length iGems)] $ \_ -> makeEmptySocket dataArea

    -- Item effects
    
    forM_ iEffects $ \mod -> 
        new #. "popeffect poptext" #= show mod #+ dataArea

    forM_ iEnchantments $ \mod ->
        new #. "popenchant poptext" #= show mod #+ dataArea

    forM_ iTriggerables $ \trig -> 
        new #. "poptriggerable poptext" #= show trig #+ dataArea


    -- Level req
    new #. "popstatreq poptext" #= show (iBaseLevelReq iBase) #+ dataArea

    when (length (iBaseOtherReqs iBase) > 0) $
        new #. "popstatreq poptext" #= "   Or" #+ dataArea # unit

    -- Stat reqs and other reqs
    forM_ (iBaseOtherReqs iBase) $ \req ->
        new #. "popstatreq poptext" #= show req #+ dataArea

    forM_ (iBaseDescription iBase) $ \line ->
        new #. "popdescription poptext" #= (show line) #+ dataArea # unit

    return titleArea #+ container
    return dataArea #+ container
    return container # setVis False # setStyle [("top", "0")] # unit
    return container #+ body # unit

makeEmptySocket container = do
    line <- new #. "popemptysocket"
    newIcon "socket_empty" #. "popemptysocketicon" #+ line
    new #. "popemptysockettext poptext" #= "Empty Socket" #+ line
    return line #+ container

makeFullSocket (icon, name, effect) container = do
    line <- new #. "popfullsocket"
    newIcon icon #. "popfullsocketicon" #+ line
    new #. "popfullsockettext poptext" #= show name #+ line
    new #. "popfullsocketeffect poptext" #= show effect #+ line
    return line #+ container

makeInnateDef (icon, descriptor) container = do
    line <- new #. "popinnatedef"
    newIcon icon #. "popinnatedeficon" #+ line
    new #. "popinnate poptext" #= show descriptor #+ line
    return line #+ container

killPopUp :: MonadTP m => m ()
killPopUp = do
    maybeP <- getElementById "itempopup"
    case maybeP of
        Nothing -> return ()
        Just p -> delete p


newIcon src =
    newImg # setSrc src # blockDrag

setSrc src = \x -> set "src" ("static/GUIAssets/" ++ src ++ ".png") x # set "alt" src

setZ int = setStyle [("zIndex", show int)]


makeArchiveRow m (ItemSummary{..}) id = do
    row <- new #. "archiverow" ## id
    fillRow row id summaryIcon summaryName summaryStatus

    -- Set up request and event handling for popup
    onHover row $ \_ -> killPopUp >> (liftIO $ writeFMessage m $ RequestItem row (Archive summaryDbID))
    onBlur row $ \_ -> killPopUp
    onDragStart row $ \_ -> killPopUp
    return row

setColorBy Archived = setStyle [("color", "black")]
setColorBy _        = setStyle [("color", "gray")]

fillRow row id icon name status = do
    let setColor = setColorBy status
    iconCell <- new #. "archivecell iconcell" # setColor ## (id ++ "icon")
    iconEl <- newIcon icon #. "archiveicon" ## (id++"iconimg") # blockDrag
              # set "onmousedown" "event.preventDefault()" #+ iconCell
    when (status == Archived) $
         return iconEl # setDragData id # allowDrag # set "onmousedown" "" # setColor # unit
    nameCell <- new #. "archivecell namecell" # setColor ## (id ++ "name")
    locCell  <- new #. "archivecell statuscell" # setColor ## (id ++ "status")

    return iconCell #+ row
    return nameCell #= name #+ row
    return locCell #= show status #+ row

updateArchiveRow rowEl id (Just (item@Item{..})) = do
    let Descriptor n _ _ = iName
    --traceShow ("=============== Looking for ID J " ++ id) $ return ()
    emptyEl rowEl >> fillRow rowEl id (iBaseIcon iBase) n Archived # unit
    
updateArchiveRow rowEl id (Nothing) = do
    --traceShow ("=============== Looking for ID N " ++ id) $ return ()
    cells <- getElementsById $ map (id++) ["iconimg", "icon", "name", "status"]
    forM_ cells $ \c -> return c # setColorBy Stashed # unit
    return (cells !! 0) # blockDrag # set "onmousedown" "event.preventDefault()" # unit
    return (cells !! 3) #= "Stashed" # unit

