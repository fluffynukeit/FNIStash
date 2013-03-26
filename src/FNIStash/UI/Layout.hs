-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.UI.Layout
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

module FNIStash.UI.Layout (
    stash,
    insertAt
) where

import FNIStash.UI.Icon
import FNIStash.File.Location

import Graphics.UI.Ji.Browser
import Graphics.UI.Ji

import Control.Monad

locIdGenerator :: Location -> (Int -> String)
locIdGenerator loc = \x -> locContainer loc ++ ":" ++ locSlot loc ++ ":" ++ (show $ x)

locToId :: Location -> String
locToId loc = locIdGenerator loc $ locIndex loc

sharedStashArms = (5, 8, Location "SHARED_STASH_BAG_ARMS" "BAG_ARMS_SLOT" 0, "ig_inventorytabs_arms")
sharedStashCons = (5, 8, Location "SHARED_STASH_BAG_CONSUMABLES" "BAG_CONSUMABLES_SLOT" 0, "ig_inventorytabs_consumables")
sharedStashSpells = (5, 8, Location "SHARED_STASH_BAG_SPELLS" "BAG_SPELL_SLOT" 0, "ig_inventorytabs_spells")

stash _ = do
    cont <- new ## "sharedstash_div"
    newIcon "ig_merchant_menu_base" ## "sharedstash_img" #+ cont
    tabbedGrid sharedStashArms #+ cont
    tabbedGrid sharedStashCons #+ cont
    tabbedGrid sharedStashSpells #+ cont
    return cont

tabbedGrid (r, c, loc, im) = do
    g <- grid r c (locIdGenerator loc)
    i <- newIcon (im ++ "_unselected_") #. "inventory_tab"  ## im
    return i #+ g # unit
    onClick i (\_ -> return i # setSrc (im ++ "_selected_") # unit)
    return g

grid r c gen = do
    let rowStarts = [0, c .. r*c-1]
    grid <- new #. "grid"
    mapM (\startId -> gridRow startId c gen #+ grid) rowStarts
    return grid

gridRow startId n gen = do
    let idList = [startId..startId+n-1]
    row <- (new #. "gridrow")
    mapM (\id -> gridCell id gen #+ row) idList
    return row


gridCell id gen = new ## (gen id) #. "gridcell"

insertAt loc itemElems = do
    mEl <- getElementById (locToId loc)
    case mEl of
        Just el -> return itemElems #+ el # unit
        Nothing -> return ()
