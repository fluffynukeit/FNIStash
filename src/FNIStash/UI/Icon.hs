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

import Graphics.UI.Ji
import Graphics.UI.Ji.Browser


newItemIcon item = do
    p <- itemPopup item
    i <- newIcon (itemIcon item) (itemIcon item)
        #. "item"
        # set "draggable" "true"
        # add p
    new # add i # add p

itemPopup item = new #. "itempopup" #= (showItem item)

newIcon src alt =
    newImg
    # set "src" ("static/GUIAssets/" ++ src ++ ".png")
    # set "alt" alt


