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

module FNIStash.UI.Icon
where

import FNIStash.File.SharedStash

import Graphics.UI.Threepenny
import Graphics.UI.Threepenny.Browser

import Control.Monad

newItemIcon item = do
    i <- newIcon (itemIcon item)
        #. "item"
        # allowDrag
    onHover i $ \_ -> makePopUp item
    onBlur i $ \_ -> killPopUp
    return i

makePopUp item = do
    let displayText = showItem item
    body <- getBody
    container <- new #. "itempopup" ## "itempopup"
    new #. "poplevel" #= "Level " ++ (show.itemLevel) item #+ container
    newIcon (itemIcon item) #. "popicon" #+ container
    new #. "poptitle" #= itemName item #+ container
    new #. "poppoints" #= (show.itemPoints) item #+ container
    forM_ (itemMods item) $ \mod -> do
        new #. "popmod" #= showMod mod #+ container
    new #. "popnumenchants" #= (show.itemNumEnchants) item #+ container
    return container #+ body # unit



killPopUp :: MonadTP m => m ()
killPopUp = do
    maybeP <- getElementById "itempopup"
    case maybeP of
        Nothing -> return ()
        Just p -> delete p

itemPopup item = new #. "itempopup" #= (showItem item)

newIcon src =
    newImg
    # setSrc src

setSrc src = \x -> set "src" ("static/GUIAssets/" ++ src ++ ".png") x # set "alt" src

setZ int = set "style" ("z-index:" ++ show int ++ ";")


