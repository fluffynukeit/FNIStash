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
    updateCell
) where

import FNIStash.UI.Icon
import FNIStash.File.Location
import FNIStash.Comm.Messages

import Graphics.UI.Threepenny.Browser
import Graphics.UI.Threepenny

import Control.Monad
import Control.Monad.Trans
import Data.Maybe

sharedStashArms = (Location "SHARED_STASH_BAG_ARMS" "BAG_ARMS_SLOT" 0, "ig_inventorytabs_arms")
sharedStashCons = (Location "SHARED_STASH_BAG_CONSUMABLES" "BAG_CONSUMABLES_SLOT" 0, "ig_inventorytabs_consumables")
sharedStashSpells = (Location "SHARED_STASH_BAG_SPELLS" "BAG_SPELL_SLOT" 0, "ig_inventorytabs_spells")

stash mes = do
    cont <- new ## "sharedstash_div"
    newIcon "ig_merchant_menu_base" ## "sharedstash_img" #+ cont
    let gStack = tabbedGridStack mes 5 8 [sharedStashArms, sharedStashCons, sharedStashSpells]
    gStack ## "sharedstash_stack" #+ cont
    return cont

tabbedGridStack messages r c templates = do
    div <- new #. "tabbed_grid_stack"
    -- make a triplet of grid, tab for grid, and tab image prefix
    gridTabTrips <- forM templates (\(loc, tabImg) -> do
        trip1 <- grid messages r c (locIdGenerator loc)
        trip2 <- newIcon (tabImg ++ "_unselected_") #. "inventory_tab" ## tabImg
        return (trip1, trip2, tabImg)
        )
    -- divide triplets up into "A" and "not A", for each triplet
    let gridCombos = complements gridTabTrips
    -- for each divided triplet
    forM_ gridCombos (\((g,t,i), otherPairs) -> do
        -- set up click handlers and append to container
        onClick t (\_ -> do
            -- set all other grids to background and make their tabs unselected
            forM_ otherPairs (\(og, ot, oi) -> do
                return og # setVis False # unit
                return ot # setSrc (oi ++ "_unselected_") # unit)
            -- bring this grid to the foreground and select its tab
            return g # setVis True # unit
            return t # setSrc (i ++ "_selected_") # unit)
        -- insert the grid and the tab into the containing div
        return g #+ div
        return t #+ div
        )
    -- now we need to pick a grid to start out as the "top" grid
    -- set the tail grids to lower z index to start with
    forM (tail gridTabTrips) (\(grid, _, _) ->
        return grid # setVis False # unit)
    -- set the top most grid to a higher z index and return it
    let (gTop, tabTop, srcTop) = head gridTabTrips
    return gTop # setVis True # unit
    return tabTop # setSrc (srcTop ++ "_selected_") # unit
    return div

-- For each element in a list, returns the pair of the element and the other elements.
complements list =
    let indices = [0..]
        pairList = zip indices list
        elemsNotMe (i, e) = (e, map snd $ filter (\(k,_) -> i /= k) pairList)
    in map elemsNotMe pairList


grid messages r c gen = do
    let rowStarts = [0, c .. r*c-1]
    grid <- new #. "grid"
    mapM (\startId -> gridRow messages startId c gen #+ grid) rowStarts
    return grid

gridRow messages startId n gen = do
    let idList = [startId..startId+n-1]
    row <- (new #. "gridrow")
    mapM (\id -> gridCell messages id gen #+ row) idList
    return row


gridCell messages id gen = do
    let idString = gen id
    d <- new ## idString #. "gridcell" # allowDrop
    onDragEnter d $ \_ -> set "style" "background-color:#ffff99;" d # unit
    onDragLeave d $ \_ -> set "style" "background-color:transparent;" d # unit
    onDragEnd d $ \_ -> set "style" "background-color:transparent;" d # unit
    onDrop d $  \(EventData eData) -> do
        set "style" "background-color:transparent;" d # unit
        liftIO (notifyMove messages eData idString)
    return d

notifyMove mes eData toId = do
    let fromId = fromJust $ head eData
        from = idToLoc fromId
        to = idToLoc toId
    writeFMessage mes $ Move from to

updateCell loc mItem = do
    let id = locToId loc
    mEl <- getElementById id
    maybe (trace ("ID not found!!!: " ++ id) return ()) (\el -> do
        case mItem of
            Just item   -> do
                emptyEl el
                newItemIcon item # setDragData id #+ el # unit
            Nothing     -> emptyEl el # unit)
        mEl
