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
    body <- getBody
    container <- new #. "itempopup" ## "itempopup"
    new #. "poplevel" #= "Level " ++ (show.itemLevel) item #+ container
    newIcon (itemIcon item) #. "popicon" #+ container
    new #. "poptitle" #= itemName item #+ container
    new #. "poppoints" #= (show.itemPoints) item #+ container
    forM_ (itemEffects item) $ \eff -> do
        new #. "popeffect" #= show eff #+ container
    new #. "popenchant" #= "Num enchants: " ++ ((show.itemNumEnchants) item) #+ container
    forM_ (itemTriggerables item) $ \trig -> do
        new #. "poptriggerable" #= show trig #+ container
    forM_ (itemStats item) $ \stat -> do
        new #. "popstat" #= show stat #+ container
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


