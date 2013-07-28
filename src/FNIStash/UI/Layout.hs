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
, updateCell
, withLocVals
, populateArchiveTable
, locToId
, updateReport
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
import Data.List (zip4)

import Debug.Trace

sharedStashArms = (Location "SHARED_STASH_BAG_ARMS" "BAG_ARMS_SLOT", "ig_inventorytabs_arms")
sharedStashCons = (Location "SHARED_STASH_BAG_CONSUMABLES" "BAG_CONSUMABLES_SLOT", "ig_inventorytabs_consumables")
sharedStashSpells = (Location "SHARED_STASH_BAG_SPELLS" "BAG_SPELLS_SLOT", "ig_inventorytabs_spells")

locTemplates = [sharedStashArms, sharedStashCons, sharedStashSpells]

locIdGenerator :: Location -> String -> String
locIdGenerator loc = \x -> locContainer loc ++ ":" ++ x

locToId :: Location -> String
locToId (loc@Location{..}) = locIdGenerator loc $ show locIndex
locToId (Archive id)   = "ARCHIVE:" ++ (show id)
locToId (InsertedInSocket) = "ErrorInsertedID"

idToLoc :: String -> Location
idToLoc id =
    let (a:c:rest) = splitOn ":" id
        matchingSlot "SHARED_STASH_BAG_ARMS" = "BAG_ARMS_SLOT"
        matchingSlot "SHARED_STASH_BAG_CONSUMABLES" = "BAG_CONSUMABLES_SLOT"
        matchingSlot "SHARED_STASH_BAG_SPELLS" = "BAG_SPELLS_SLOT"
    in case a of
        "ARCHIVE" -> Archive (read c)
        _         -> Location a (matchingSlot a) (read c)


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
    let gStack = tabbedGridStack mes 5 8 locTemplates
    gStack ## "sharedstash_stack" #+ cont

    batchArchiveButton mes #+ cont

    (updateBtn, txt) <- updateStashButton mes
    return updateBtn #+ cont

    (report, repTxt) <- mkReport
    return report #+ cont
    
    return (cont, txt, repTxt)

tabbedGridStack messages r c templates = do
    div <- new #. "tabbed_grid_stack"
    -- make a triplet of grid, tab for grid, and tab image prefix
    gridTabTrips <- forM templates (\(loc, tabImg) -> do
        trip1 <- grid messages r c (locIdGenerator $ loc 0)
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
    archiveEl <- archive messages (gen "ARCHIVE")
    return archiveEl #+ cont
    return cont

archive msg bodyID = do
    archiveEl <- new #. "archive" 
    archiveText <- new #. "archivetitle" #= "Registry"
    aBody <- new #. "archivetablecontainer" # allowDrop
    archiveBody <- new #. "archivetable" ## bodyID

    return archiveText #+ archiveEl
    return archiveBody #+ aBody
    return aBody #+ archiveEl

    -- Set up drop functionality
    onDragEnter aBody $ \_ ->
        setStyle [("outline", "thick solid #ffff99")] aBody # unit
    onDragLeave aBody $ \_ -> setStyle [("outline", "thick none #ffff99")] aBody # unit
    onDragEnd aBody $ \_ -> setStyle [("outline", "thick none #ffff99")] aBody # unit
    onDrop aBody $ \(EventData eData) ->
        setStyle [("outline", "thick none #ffff99")] aBody >> processDrop msg "ARCHIVE:-1" eData
    return archiveEl

gridRow messages startId n gen = do
    let idList = [startId..startId+n-1]
    row <- (new #. "gridrow")
    mapM (\id -> gridCell messages id gen #+ row) idList
    return row


gridCell messages id gen = do
    let idString = gen (show id)
    d <- new ## idString #. "gridcell" # allowDrop
    onDragEnter d $ \_ -> setDragColor d # unit
    onDragLeave d $ \_ -> setTrans d # unit
    onDragEnd d $ \_ -> setTrans d # unit
    onDrop d $  \(EventData eData) -> setTrans d >> processDrop messages idString eData
    return d
    where
        setTrans     = setStyle [("backgroundColor", "transparent")]
        setDragColor = setStyle [("backgroundColor", "#ffff99")]
       
processDrop _ _ (Nothing:_) = return ()
processDrop messages dropID ((Just fromID):_)
    | (take 12 fromID) == "SHARED_STASH" = 
        liftIO $ notifyMove messages fromID dropID
    | (take 7 fromID) == "ARCHIVE" =
        liftIO $ notifyMove messages fromID dropID
    | otherwise =
        return () --- do nothing


notifyMove mes eString toId = do
    let fromId = eString
        from = idToLoc fromId
        to = idToLoc toId
        mvMsg = Move [(from, to)]
    writeFMessage mes mvMsg

notifySave mes = writeFMessage mes Save
notifySearch mes str = writeFMessage mes $ Search str
notifyBatchArchive mes =
    let applyToIndices k = map k [0..39]
        allLocations = concat $ map (applyToIndices.fst) locTemplates
        moveCommands = zip allLocations $ repeat (Archive (-1))
    in writeFMessage mes $ Move moveCommands

showReport = return ()


withLocVals locValList actionOfElValId = do
    let locs = map fst locValList
        ids = map locToId locs
    els <- getElementsById ids
    let tuples = zip4 els locs (map snd locValList) ids
    forM_ tuples $ \(e,l,v,i) -> actionOfElValId e l v i


updateCell el (Location _ _ _) (Just item) id =
    emptyEl el >> newItemIcon item # setDragData id #+ el # unit
updateCell el (Location _ _ _) Nothing id =
    emptyEl el # unit
updateCell el (Archive _) mItem id =
    updateArchiveRow el id mItem

populateArchiveTable m summs =
    let armsSumms = filter ((== Arms) . summaryItemClass) summs
        consSumms = filter ((== Consumables) . summaryItemClass) summs
        spellsSumms = filter ((== Spells) . summaryItemClass) summs
    in do
        (armsTab:consTab:spellsTab:_) <- getElementsById $ map (flip locIdGenerator "ARCHIVE")
            $ map (flip ($) 0 . fst) locTemplates

        appendArchiveRows m armsTab armsSumms
        appendArchiveRows m consTab consSumms
        appendArchiveRows m spellsTab spellsSumms

appendArchiveRows m table summs = forM_ summs $ \(i@ItemSummary{..}) ->
    makeArchiveRow m i (locToId $ Archive summaryDbID) #+ table

makeButton offImg overImg explanation clickAction = do
    cont <- new #. "imgbutton" # set "title" explanation
    btn <- newIcon offImg #. "imgbuttonicon"
    onHover cont $ \_ -> setSrc overImg btn # unit
    onBlur  cont $ \_ -> setSrc offImg btn # unit
    onClick cont $ \_ -> liftIO $ clickAction
    text <- new #. "imgbuttontext"
    return btn #+ cont
    return text #+ cont
    return (cont, text)


batchArchiveButton mes = do
    (btn, txt) <- makeButton "arrow_down" "arrow_down_highlight"
                  "Archive all"
                  (notifyBatchArchive mes)
    return btn ## "batcharchivebutton"

makeRedButton = makeButton "ig_abandon_button" "ig_abandon_button_rollover"

updateStashButton mes = do
    (btn, txt) <- makeRedButton
                  "Updates TL2 shared stash with items shown"
                  (notifySave mes)
    return btn ## "updatestashbutton"
    return txt #= "Update Stash"
    return (btn, txt)

mkReport = do
    (btn, txt) <- makeRedButton "Shows progress of finding all items" showReport
    return btn ## "reportbutton"
    return (btn, txt)

updateReport a ItemsReport{..} =
    return a #= "Grail: " ++ (take 5 $ show reportPercentFound) ++ "%" # unit
    

