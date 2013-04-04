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

import Graphics.UI.Threepenny.Browser
import Graphics.UI.Threepenny

import Control.Monad

locIdGenerator :: Location -> (Int -> String)
locIdGenerator loc = \x -> locContainer loc ++ ":" ++ locSlot loc ++ ":" ++ (show $ x)

locToId :: Location -> String
locToId loc = locIdGenerator loc $ locIndex loc

sharedStashArms = (Location "SHARED_STASH_BAG_ARMS" "BAG_ARMS_SLOT" 0, "ig_inventorytabs_arms")
sharedStashCons = (Location "SHARED_STASH_BAG_CONSUMABLES" "BAG_CONSUMABLES_SLOT" 0, "ig_inventorytabs_consumables")
sharedStashSpells = (Location "SHARED_STASH_BAG_SPELLS" "BAG_SPELL_SLOT" 0, "ig_inventorytabs_spells")

stash _ = do
    cont <- new ## "sharedstash_div"
    newIcon "ig_merchant_menu_base" ## "sharedstash_img" #+ cont
    let gStack = tabbedGridStack 5 8 [sharedStashArms, sharedStashCons, sharedStashSpells]
    gStack ## "sharedstash_stack" #+ cont
    return cont

tabbedGridStack r c templates = do
    div <- new #. "tabbed_grid_stack"
    -- make a triplet of grid, tab for grid, and tab image prefix
    gridTabTrips <- forM templates (\(loc, tabImg) -> do
        trip1 <- grid r c (locIdGenerator loc)
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

--tabbedGrid (r, c, loc, im) otherPages = do
--    div <- new #. "tabbedgrid"
--    g <- grid r c (locIdGenerator loc)
--    i <- newIcon (im ++ "_unselected_") #. "inventory_tab"  ## im
--    return g #+ div # unit
--    return i #+ div # unit
--    onClick i (\_ -> do
--        return i # setSrc (im ++ "_selected_") # unit
--        return g # set "style" "z-index:10;" # unit
--        forM_ otherPages (\other -> return other # set "style" "z-index:-1;" # unit))
--    return g

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


gridCell id gen = do
    d <- new ## (gen id) #. "gridcell"
    onDragEnter d $ \_ -> set "style" "background-color:#ffff99;" d # unit
    onDragLeave d $ \_ -> set "style" "background-color:transparent;" d # unit
    return d


insertAt loc itemElems = do
    mEl <- getElementById (locToId loc)
    case mEl of
        Just el -> return itemElems #+ el # unit
        Nothing -> return ()
