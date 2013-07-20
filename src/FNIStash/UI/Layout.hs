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

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}

module FNIStash.UI.Layout
( stash
, controls
, overlay
, updateItem
, withLocVals
, populateArchiveTable
) where

import FNIStash.UI.Icon
import FNIStash.Comm.Messages
import FNIStash.UI.Effects

import Graphics.UI.Threepenny.Browser
import Graphics.UI.Threepenny.Elements
import Graphics.UI.Threepenny

import Control.Monad
import Control.Monad.Trans
import Data.Maybe
import Data.List.Split

import Debug.Trace

sharedStashArms = (Location "SHARED_STASH_BAG_ARMS" "BAG_ARMS_SLOT" 0, "ig_inventorytabs_arms")
sharedStashCons = (Location "SHARED_STASH_BAG_CONSUMABLES" "BAG_CONSUMABLES_SLOT" 0, "ig_inventorytabs_consumables")
sharedStashSpells = (Location "SHARED_STASH_BAG_SPELLS" "BAG_SPELLS_SLOT" 0, "ig_inventorytabs_spells")

locIdGenerator :: Location -> String -> String
locIdGenerator loc = \x -> locContainer loc ++ ":" ++ x

locToId :: Location -> String
locToId loc = locIdGenerator loc $ show $ locIndex loc

idToLoc :: String -> Location
idToLoc id =
    let (a:c:rest) = splitOn ":" id
        matchingSlot "SHARED_STASH_BAG_ARMS" = "BAG_ARMS_SLOT"
        matchingSlot "SHARED_STASH_BAG_CONSUMABLES" = "BAG_CONSUMABLES_SLOT"
        matchingSlot "SHARED_STASH_BAG_SPELLS" = "BAG_SPELLS_SLOT"
    in Location a (matchingSlot a) (read c)


overlay = do
    overlayLogo <- new ## "overlaylogo" #= "FNIStash":: TP Element
    overlayMsg <- new ## "overlaymsg"
    overlayContent <- new ## "overlaycontent" #
                      add overlayLogo #
                      add overlayMsg
    overlay <- new ## "overlay" # add overlayContent
    return (overlay, overlayMsg)

controls mes body = do
    controls <- new
    msgWindow <- new ## "msgwindow" #+ controls
    saveButton <- new #= "Click here to save" #+ controls
    onClick saveButton $ \_ -> liftIO $ notifySave mes
    searchBox <- newTextarea #. "searchbox"
    onSendValue searchBox $ \content -> liftIO $ notifySearch mes content
    searchLabel <- new #. "searchlabel" #= "Filter:"
    searchPanel <- new #. "searchpanel" #
                    add searchLabel #
                    add searchBox #+
                    controls
    return controls #+ body
    return msgWindow

stash mes = do
    cont <- new ## "sharedstash"
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

    cont <- new #. "gridandarchive"
    return grid #+ cont
    archiveEl <- archive (gen "ARCHIVE")
    return archiveEl #+ cont
    return cont

archive bodyID = do
    archiveEl <- new #. "archive"
    archiveText <- new #. "archivetitle" #= "Registry"
    archiveBodyContainer <- new #. "archivetablecontainer"
    archiveBody <- new #. "archivetable" ## bodyID

    return archiveText #+ archiveEl
    return archiveBody #+ archiveBodyContainer
    return archiveBodyContainer #+ archiveEl
    return archiveEl

gridRow messages startId n gen = do
    let idList = [startId..startId+n-1]
    row <- (new #. "gridrow")
    mapM (\id -> gridCell messages id gen #+ row) idList
    return row


gridCell messages id gen = do
    let idString = gen (show id)
    d <- new ## idString #. "gridcell" # allowDrop
    onDragEnter d $ \_ -> setColor d # unit
    onDragLeave d $ \_ -> setTrans d # unit
    onDragEnd d $ \_ -> setTrans d # unit
    onDrop d $  \(EventData eData) -> setTrans d >> processDrop idString d eData
    return d
    where
        setTrans = set "style" "background-color:transparent;" -- for some reason, setStyle isn't working for these
        setColor = set "style" "background-color:#ffff99;"
        processDrop _ _ (Nothing:_) = return ()
        processDrop idString d ((Just eString):_)
            | (take 12 eString) == "SHARED_STASH" = 
                liftIO (notifyMove messages eString idString)
            | (take 7 eString) == "ARCHIVE" =
                return () --- do nothing for now
            | otherwise =
                return () --- do nothing



notifyMove mes eString toId = do
    let fromId = eString
        from = idToLoc fromId
        to = idToLoc toId
    writeFMessage mes $ Move from to

notifySave mes = writeFMessage mes Save
notifySearch mes str = writeFMessage mes $ Search str

withLocVals locValList actionOfElValId = do
    let ids = map (locToId.fst) locValList
    els <- getElementsById ids
    let tuples = zip3 els (map snd locValList) ids
    forM_ tuples $ \(e,v,i) -> actionOfElValId e v i

updateItem el mItem id = do
    case mItem of
        Just item   -> do
            emptyEl el
            newItemIcon item # setDragData id #+ el # unit
        Nothing     -> emptyEl el # unit

populateArchiveTable m summs =
    let armsSumms = filter ((== Arms) . summaryItemClass) summs
        consSumms = filter ((== Consumables) . summaryItemClass) summs
        spellsSumms = filter ((== Spells) . summaryItemClass) summs
    in do
        (armsTab:consTab:spellsTab:_) <- getElementsById $ map (flip locIdGenerator "ARCHIVE" . fst)
            [sharedStashArms, sharedStashCons, sharedStashSpells]

        appendArchiveRows m armsTab armsSumms
        appendArchiveRows m consTab consSumms
        appendArchiveRows m spellsTab spellsSumms

appendArchiveRows m table pairs = forM_ pairs $ \pair -> makeArchiveRow m pair #+ table


