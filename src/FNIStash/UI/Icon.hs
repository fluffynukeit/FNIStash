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

import Control.Monad
import Data.Maybe

for = flip map

newItemIcon (item@Item {..}) = do
    container <- new # set style [("position", "relative")]
    
    i <- newIcon (iBaseIcon iBase)
        #. "item"
        # set draggable True
    element container #+ [element i]

    -- create full and empty socket icons
    let numFullSockets = length iGems
    element container #+ (for [1..numFullSockets] $ \ind -> 
        newIcon "socket_full" #. ("socket" ++ show ind) )

    element container #+ (for [numFullSockets+1..iNumSockets] $ \ind ->
        newIcon "socket_empty" #. ("socket" ++ show ind) )

    w <- liftM getWindow (element container)

    on hover container $ \_ -> do
        killPopUp w
        body <- getBody w
        element body #+ [popUp item]
        set (attr "onmousemove") moveScript (element container)

    on blur container $ \_ -> killPopUp w
    
    return container

moveScript = "if (event.clientX < document.body.clientWidth/2) \
             \{document.getElementById(\"itempopup\").style.left=(event.clientX)+\"px\";}\
             \else {document.getElementById(\"itempopup\").style.right=(document.body.clientWidth - event.clientX)+\"px\";}\
             \document.getElementById(\"itempopup\").style.top=(event.clientY+5)+\"px\""


popUp (Item{..}) = do

    container <- new #. "itempopup" # set (attr "id") "itempopup"
    titleArea <- new #. "poptitlearea"
    element titleArea #+ [ new #. "poplevel" # set text ("Level " ++ show iLevel)
                         , newIcon (iBaseIcon iBase) #. "popicon"]

    let qualityClass = case (uQuality . iBaseUnitType $ iBase) of
            NormalQ     -> "popnormal"
            MagicQ      -> "popmagic"
            UniqueQ     -> "popunique"
            LegendaryQ  -> "poplegendary"
            _           -> "popnormal"
    
    element titleArea #+ [new #. qualityClass # set text iName]

    dataArea <- new #. "popdataarea"
    element dataArea #+ (for (iBaseInnates iBase) $ \inn ->
        new #. "popinnate" # set text (show inn))

    forM_ (iInnateDefs) $ \pair -> makeInnateDef pair dataArea


    -- Sockets
    forM_ iGems $ \gem -> makeFullSocket gem dataArea

    forM_ [1..(iNumSockets - length iGems)] $ \_ -> makeEmptySocket dataArea

    -- Item effects
    
    element dataArea #+ (for iEffects $ \mod -> 
        new #. "popeffect" # set text (show mod))

    element dataArea #+ (for iEnchantments $ \mod ->
        new #. "popenchant" # set text (show mod))

    element dataArea #+ (for iTriggerables $ \trig -> 
        new #. "poptriggerable" # set text (show trig))


    -- Level req
    element dataArea #+ [new #. "popstatreq" # set text (show (iBaseLevelReq iBase))]

    when (length (iBaseOtherReqs iBase) > 0) $
        element dataArea #+ [new #. "popstatreq" # set text "Or"] >> return ()

    -- Stat reqs and other reqs
    element dataArea #+ (for (iBaseOtherReqs iBase) $ \req ->
        new #. "popstatreq" # set text (show req))

    when (isJust $ iBaseDescription iBase) $ void $
        element dataArea #+ (for (fromJust $ iBaseDescription iBase) $ \line ->
            new #. "popdescription" # set text (show line))

    element container #+ [ element titleArea
                         , element dataArea]

    return container

makeEmptySocket container = do
    line <- new #. "popemptysocket"
    element line #+ [ newIcon "socket_empty" #. "popemptysocketicon"
                    , new #. "popemptysockettext" # set text "Empty Socket"]
    element container #+ [element line]

makeFullSocket (icon, name, effect) container = do
    line <- new #. "popfullsocket"
    element line #+ [ newIcon icon #. "popfullsocketicon"
                    , new #. "popfullsockettext" # set text (show name)
                    , new #. "popfullsocketeffect" # set text (show effect)]
    element container #+ [element line]

makeInnateDef (icon, descriptor) container = do
    line <- new #. "popinnatedef"
    element line #+ [ newIcon icon #. "popinnatedeficon"
                    , new #. "popinnate" # set text (show descriptor)]
    element container #+ [element line]


killPopUp w = do
    
    maybeP <- getElementById w "itempopup"
    case maybeP of
        Nothing -> return ()
        Just p -> delete p


newIcon src = 
    img
    # setSrc src

setSrc src = \x -> set (attr "src") ("static/GUIAssets/" ++ src ++ ".png") x # set (attr "alt") src

setZ int = set style [("z-index", show int)]


