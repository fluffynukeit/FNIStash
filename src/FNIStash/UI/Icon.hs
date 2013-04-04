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


newItemIcon item = do
    i <- newIcon (itemIcon item)
        #. "item"
        # set "draggable" "true"
    return i

itemPopup item = new #. "itempopup" #= (showItem item)

newIcon src =
    newImg
    # setSrc src

setSrc src = \x -> set "src" ("static/GUIAssets/" ++ src ++ ".png") x # set "alt" src

setZ int = set "style" ("z-index:" ++ show int ++ ";")

setVis v = set "style" ("visibility:" ++
    case v of
        True    -> "visible;"
        False   -> "hidden;"
        )

-- Drag events
onDragStart :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragStart = bind "dragstart"

onDragEnter :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragEnter = bind "dragenter"

onDragOver :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragOver = bind "dragover"

onDragLeave :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragLeave = bind "dragleave"

onDrag :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDrag = bind "drag"

onDragDrop :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragDrop = bind "drop"

onDragEnd :: MonadTP m => Element -> (EventData -> m ()) -> m ()
onDragEnd = bind "dragend"
