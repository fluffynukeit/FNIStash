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

module FNIStash.UI.Icon
where

import FNIStash.File.SharedStash

import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Browser

import Control.Monad
import Data.Maybe

newItemIcon (item@Item {..}) = do
    i <- newIcon (iBaseIcon iBase)
        #. "item"
        # allowDrag
    killPopUp
    onHover i $ \_ -> makePopUp item
    onBlur i $ \_ -> killPopUp
    return i

makePopUp (Item{..}) = do
    body <- getBody
    container <- new #. "itempopup" ## "itempopup"
    new #. "poplevel" #= "Level " ++ show iLevel #+ container
    newIcon (iBaseIcon iBase) #. "popicon" #+ container

    let qualityClass = case (uQuality . iBaseUnitType $ iBase) of
            NormalQ     -> "popnormal"
            MagicQ      -> "popmagic"
            UniqueQ     -> "popunique"
            LegendaryQ  -> "poplegendary"
            _           -> "popnormal"
    
    new #. qualityClass #= iName #+ container

    forM_ (iBaseInnates iBase) $ \inn ->
        new #. "popinnate" #= show inn #+ container
    
    forM_ iEffects $ \mod -> 
        new #. "popeffect" #= show mod #+ container

    when (length iEnchantments > 0) $ do
        new #. "popenchant" #= "Enchantments: " ++ show (length iEnchantments) #+ container
        forM_ iEnchantments $ \mod ->
            new #. "popenchant" #= show mod #+ container

    forM_ iTriggerables $ \trig -> 
        new #. "poptriggerable" #= show trig #+ container
    return container #+ body # unit

    -- Level req
    new #. "popstatreq" #= show (iBaseLevelReq iBase) #+ container

    when (length (iBaseOtherReqs iBase) > 0) $
        new #. "popstatreq" #= "   Or" #+ container # unit

    -- Stat reqs and other reqs
    forM_ (iBaseOtherReqs iBase) $ \req ->
        new #. "popstatreq" #= show req #+ container

    when (isJust $ iBaseDescription iBase) $
        new #. "popdescription" #= (show $ fromJust $ iBaseDescription iBase) #+ container # unit


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


