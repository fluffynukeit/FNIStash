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

import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Browser

import Control.Monad
import Data.Maybe

newItemIcon (item@Item {..}) = do
    container <- new # setStyle [("position", "relative")]
    
    i <- newIcon (iBaseIcon iBase)
        #. "item"
        # allowDrag
    return i #+ container

    -- create full and empty socket icons
    let numFullSockets = length iGems
    forM_ [1..numFullSockets] $ \ind ->
        newIcon "socket_full" #. ("socket" ++ show ind) #+ container

    forM_ [numFullSockets+1..iNumSockets] $ \ind ->
        newIcon "socket_empty" #. ("socket" ++ show ind) #+ container

    killPopUp

    onHover container $ \_ -> killPopUp >> makePopUp item >> setAttr "onmousemove" moveScript container # unit
    onBlur container $ \_ -> killPopUp
    
    return container

moveScript = "if (event.clientX < document.body.clientWidth/2) \
             \{document.getElementById(\"itempopup\").style.left=(event.clientX)+\"px\";}\
             \else {document.getElementById(\"itempopup\").style.right=(document.body.clientWidth - event.clientX)+\"px\";}\
             \document.getElementById(\"itempopup\").style.top=(event.clientY+5)+\"px\""

makePopUp (Item{..}) = do
    body <- getBody
    container <- new #. "itempopup" ## "itempopup"
    titleArea <- new #. "poptitlearea"
    new #. "poplevel" #= "Level " ++ show iLevel #+ titleArea
    newIcon (iBaseIcon iBase) #. "popicon" #+ titleArea

    let qualityClass = case (uQuality . iBaseUnitType $ iBase) of
            NormalQ     -> "popnormal"
            MagicQ      -> "popmagic"
            UniqueQ     -> "popunique"
            LegendaryQ  -> "poplegendary"
            _           -> "popnormal"
    
    new #. qualityClass #= iName #+ titleArea

    dataArea <- new #. "popdataarea"
    forM_ (iBaseInnates iBase) $ \inn ->
        new #. "popinnate" #= show inn #+ dataArea

    forM_ (iInnateDefs) $ \pair -> makeInnateDef pair dataArea


    -- Sockets
    forM_ iGems $ \gem -> makeFullSocket gem dataArea

    forM_ [1..(iNumSockets - length iGems)] $ \_ -> makeEmptySocket dataArea

    -- Item effects
    
    forM_ iEffects $ \mod -> 
        new #. "popeffect" #= show mod #+ dataArea

    forM_ iEnchantments $ \mod ->
        new #. "popenchant" #= show mod #+ dataArea

    forM_ iTriggerables $ \trig -> 
        new #. "poptriggerable" #= show trig #+ dataArea


    -- Level req
    new #. "popstatreq" #= show (iBaseLevelReq iBase) #+ dataArea

    when (length (iBaseOtherReqs iBase) > 0) $
        new #. "popstatreq" #= "   Or" #+ dataArea # unit

    -- Stat reqs and other reqs
    forM_ (iBaseOtherReqs iBase) $ \req ->
        new #. "popstatreq" #= show req #+ dataArea

    when (isJust $ iBaseDescription iBase) $
        forM_ (fromJust $ iBaseDescription iBase) $ \line ->
            new #. "popdescription" #= (show line) #+ dataArea # unit

    return titleArea #+ container
    return dataArea #+ container
    return container #+ body # unit

makeEmptySocket container = do
    line <- new #. "popemptysocket"
    newIcon "socket_empty" #. "popemptysocketicon" #+ line
    new #. "popemptysockettext" #= "Empty Socket" #+ line
    return line #+ container

makeFullSocket (icon, name, effect) container = do
    line <- new #. "popfullsocket"
    newIcon icon #. "popfullsocketicon" #+ line
    new #. "popfullsockettext" #= show name #+ line
    new #. "popfullsocketeffect" #= show effect #+ line
    return line #+ container

makeInnateDef (icon, descriptor) container = do
    line <- new #. "popinnatedef"
    newIcon icon #. "popinnatedeficon" #+ line
    new #. "popinnate" #= show descriptor #+ line
    return line #+ container

killPopUp :: MonadTP m => m ()
killPopUp = do
    maybeP <- getElementById "itempopup"
    case maybeP of
        Nothing -> return ()
        Just p -> delete p


newIcon src =
    newImg
    # setSrc src

setSrc src = \x -> set "src" ("static/GUIAssets/" ++ src ++ ".png") x # set "alt" src

setZ int = set "style" ("z-index:" ++ show int ++ ";")


