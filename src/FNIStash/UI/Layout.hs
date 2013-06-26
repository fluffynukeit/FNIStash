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

{-# LANGUAGE ViewPatterns #-}

module FNIStash.UI.Layout
( stash
, controls
, overlay
, updateItem
, withLocVals
) where

import FNIStash.UI.Icon
import FNIStash.Comm.Messages
import FNIStash.UI.Effects
import FNIStash.Logic.Item

import Graphics.UI.Threepenny hiding (grid)
import qualified Graphics.UI.Threepenny as UI

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Data.List.Split

sharedStashArms = (Location "SHARED_STASH_BAG_ARMS" "BAG_ARMS_SLOT" 0, "ig_inventorytabs_arms")
sharedStashCons = (Location "SHARED_STASH_BAG_CONSUMABLES" "BAG_CONSUMABLES_SLOT" 0, "ig_inventorytabs_consumables")
sharedStashSpells = (Location "SHARED_STASH_BAG_SPELLS" "BAG_SPELL_SLOT" 0, "ig_inventorytabs_spells")

locIdGenerator :: Location -> (Int -> String)
locIdGenerator loc = \x -> locContainer loc ++ ":" ++ locSlot loc ++ ":" ++ (show $ x)

locToId :: Location -> String
locToId loc = locIdGenerator loc $ locIndex loc

idToLoc :: String -> Location
idToLoc id =
    let (a:b:c:rest) = splitOn ":" id
    in Location a b (read c)


overlay = do
    overlayLogo <- new # set (attr "id") "overlaylogo" # set text "FNIStash"
    overlayMsg <- new # set (attr "id") "overlaymsg"
    overlayContent <- new # set (attr "id") "overlaycontent" #+ [element overlayLogo, element overlayMsg]
    overlay <- new # set (attr "id") "overlay" #+ [element overlayContent]
    return (overlay, overlayMsg)

controls mes body = do
    controls <- new
    msgWindow <- new # set (attr "id") "msgwindow"
    saveButton <- new # set text "Click here to save"
    element controls #+ [element msgWindow, element saveButton]
    on click saveButton $ \_ -> liftIO $ notifySave mes
    searchBox <- textarea #. "searchbox"
    return $ onSendValue searchBox (\content -> notifySearch mes content)
    searchLabel <- new #. "searchlabel" # set text "Filter:"
    searchPanel <- new #. "searchpanel" #+ [element searchLabel, element searchBox]
    element controls #+ [element searchPanel]
    body #+ [element controls]
    return msgWindow

stash mes = do
    cont <- new # set (attr "id") "sharedstash"
    element cont #+ [newIcon "ig_merchant_menu_base" # set (attr "id") "sharedstash_img"]
    let gStack = tabbedGridStack mes 5 8 [sharedStashArms, sharedStashCons, sharedStashSpells]
    element cont #+ [gStack # set (attr "id") "sharedstash_stack"]
    return cont

tabbedGridStack messages r c templates = do
    div <- new #. "tabbed_grid_stack"
    -- make a triplet of grid, tab for grid, and tab image prefix
    gridTabTrips <- forM templates (\(loc, tabImg) -> do
        trip1 <- grid messages r c (locIdGenerator loc)
        trip2 <- newIcon (tabImg ++ "_unselected_") #. "inventory_tab" # set (attr "id") tabImg
        return (trip1, trip2, tabImg)
        )
    -- divide triplets up into "A" and "not A", for each triplet
    let gridCombos = complements gridTabTrips
    -- for each divided triplet
    forM_ gridCombos (\((g,t,i), otherPairs) -> do
        -- set up click handlers and append to container
        on click t (\_ -> do
            -- set all other grids to background and make their tabs unselected
            forM_ otherPairs (\(og, ot, oi) -> do
                return og # setVis False 
                return ot # setSrc (oi ++ "_unselected_") )
            -- bring this grid to the foreground and select its tab
            return g # setVis True 
            return t # setSrc (i ++ "_selected_") )
        -- insert the grid and the tab into the containing div
        element div #+ [element g, element t]
        )
    -- now we need to pick a grid to start out as the "top" grid
    -- set the tail grids to lower z index to start with
    forM (tail gridTabTrips) (\(grid, _, _) ->
        return grid # setVis False)
    -- set the top most grid to a higher z index and return it
    let (gTop, tabTop, srcTop) = head gridTabTrips
    return gTop # setVis True
    return tabTop # setSrc (srcTop ++ "_selected_") 
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
    element grid #+ map (\startId -> gridRow messages startId c gen) rowStarts
    return grid

gridRow messages startId n gen = do
    let idList = [startId..startId+n-1]
    row <- (new #. "gridrow")
    element row #+ map (\id -> gridCell messages id gen) idList
    return row


gridCell messages id gen = do
    let idString = gen id
    d <- new # set (attr "id") idString #. "gridcell" # set droppable True
    on dragEnter d $ \_ -> element d # set style [("background-color", "#ffff99")] 
    on dragLeave d $ \_ -> element d # set style [("background-color", "transparent")]
    on dragEnd d $ \_ ->   element d # set style [("background-color", "transparent")]
    on UI.drop d $  \(eData) -> do
        set style [("background-color", "transparent")] (element d)
        liftIO (notifyMove messages eData idString)
    return d

notifyMove mes eData toId = do
    let fromId = eData
        from = idToLoc fromId
        to = idToLoc toId
    writeFMessage mes $ Move from to

notifySave mes = writeFMessage mes Save
notifySearch mes str = writeFMessage mes $ Search str

withLocVals w locValList actionOfElValId = do
    let ids = map (locToId.fst) locValList
    els <- getElementsById w ids
    let tuples = zip3 els (map snd locValList) ids
    forM_ tuples $ \(e,v,i) -> withWindow w $ actionOfElValId e v i

updateItem el mItem id = do
    case mItem of
        Just item   -> do
            k <- newItemIcon item # set dragData id
            element el # set children [k]
        Nothing     -> element el # set children []
