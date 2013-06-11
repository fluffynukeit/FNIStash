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

newItemIcon (item@Item {..}) = do
    i <- newIcon (iBaseIcon iBase)
        #. "item"
        # allowDrag
    onHover i $ \_ -> makePopUp item
    onBlur i $ \_ -> killPopUp
    return i

makePopUp (Item{..}) = do
    body <- getBody
    container <- new #. "itempopup" ## "itempopup"
    new #. "poplevel" #= "Level " ++ show iLevel #+ container
    newIcon (iBaseIcon iBase) #. "popicon" #+ container
    new #. "poptitle" #= iName #+ container
    new #. "poppoints" #= show iPoints #+ container
    forM_ iEffects $ \mod -> 
        new #. "popeffect" #= show mod #+ container
    new #. "popenchant" #= "Num enchants: " ++ show (length iEnchantments) #+ container

    -- Stat reqs
    forM_ (iBaseStatReqs iBase) $ \req ->
        new #. "popstatreq" #= "Required " ++ show req #+ container

    forM_ iTriggerables $ \trig -> 
        new #. "poptriggerable" #= show trig #+ container
    return container #+ body # unit



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


