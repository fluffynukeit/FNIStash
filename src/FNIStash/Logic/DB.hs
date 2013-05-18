-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.Logic.DB
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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FNIStash.Logic.DB
( handleDBError
, initializeDB
, register
)
where

import FNIStash.File.Item
import FNIStash.Logic.Env
import FNIStash.Logic.Translate

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Monoid
import Data.Time.LocalTime
import Data.Maybe
import Control.Monad
import Control.Applicative
import qualified Data.List as L
import Data.Convertible.Base
import GHC.Float

import Filesystem.Path.CurrentOS
import Filesystem

import Debug.Trace

handleDBError io = catchSql io (\err -> trace ("**SQL ERROR**: " ++ show err) return ())

initializeDB appRoot = do
    let dbPath = appRoot </> "fnistash.db"
    dbExists <- isFile dbPath
    conn <- connectSqlite3 $ encodeString dbPath

    -- need to enable foreign keys for each connection
    run conn "PRAGMA foreign_keys = ON;" []

    if dbExists then
        return () -- don't need to make a new table
        else
            setUpAllTables conn >> commit conn

    return conn

-- Given a list of items, registers those that have not been registered yet and
-- returns the newly registered items in a list
register env items = do
    let conn = dbConn env
    succItems <- fmap catMaybes $ forM items $ \item@(Item {..}) -> do
        wasRegistered <- isRegistered env item

        if wasRegistered then
            return Nothing
            else do
            addItemToDB env item
            return $ Just item
            
    return succItems

isRegistered env (Item {..}) = do
    matchingGuys <- quickQuery' (dbConn env) "select RANDOM_ID from ITEMS where RANDOM_ID = ?" [toSql itemRandomID]
    if length matchingGuys == 0 then return False else return True

addItemToDB env item@(Item {..}) = let c  = dbConn env in withTransaction c $ \conn -> do
    -- First register the item data, starting with the Trail data
    trailID <- insertTrailData conn item
    itemID <- insertItem conn item trailID
    descList <- sequence (
        [ insertDescriptor conn Name itemName 0
        , insertDescriptor conn Level "Level VALUE" itemLevel
        ] ++
        L.map (\mod -> insertDescriptor conn Mod (translateSentence mod $ modText mod) (modValue mod)) itemMods
        )
    insertDescriptorSet conn itemID descList




-- Use like this: ensureExists conn "ITEMS" ["RANDOM_ID", "GUID"] [toSql itemRandomID, toSql itemGUID]
-- Tries to do an insert and ignores if constraint is broken.  Returns the ID of the row with
-- head of cols matching head of values
ensureExists conn table cols vals = do
    let stringListToTupleString strings = "(" <> (L.intercalate "," strings) <> ")"
        colsTuple = stringListToTupleString cols
        marksTuple = stringListToTupleString $ replicate (length vals) "?"
        insertQ = "insert or ignore into " <> table <> colsTuple <> " values " <> marksTuple
        selectQ = "select ID from " <> table <> " where " <> (head cols) <> " = (?)"
    --traceShow (insertQ, selectQ, cols, vals) $ return ()
    run conn insertQ vals
    idList <- quickQuery' conn selectQ [head vals]
    --traceShow (L.concat idList) $ return ()
    (return $ fromSql $ (head . L.concat) idList) :: IO (ID a)


--insertTrailData :: IConnection a => a -> Item -> IO (ID TrailData)
insertTrailData conn item@(Item {..}) =
    ensureExists conn "TRAIL_DATA" ["DATA"] [toSql $ itemTrailData item]

--insertItem :: IConnection a => a -> Item -> ID TrailData -> IO (ID Items)
insertItem conn item@(Item {..}) trailDataID = do
    zonedTime <- getZonedTime
    let localTime = zonedTimeToLocalTime zonedTime

    ensureExists conn "ITEMS" ["RANDOM_ID", "GUID", "CONTAINER", "SLOT", "POSITION", "LEAD_DATA", "FK_TRAIL_DATA_ID", "DATE"]
        [ toSql itemRandomID, toSql itemGUID, toSql (locContainer itemLocation), toSql (locSlot itemLocation)
        , toSql (locIndex itemLocation), toSql (itemLeadData item), toSql trailDataID, toSql localTime]

--insertDescriptor :: IConnection a => a -> DescriptorType -> String -> Float-> IO (ID Descriptors, Float)
insertDescriptor conn descType desc val = do
    id <- ensureExists conn "DESCRIPTORS" ["EXPRESSION", "TYPE"] [toSql $ (desc::String), toSql descType]
    return (id, show val)

--insertDescriptorSet :: IConnection a => a -> ID Items -> [(ID Descriptors, Float)] -> IO ()
insertDescriptorSet conn itemID descIDValPairs =
    forM_ descIDValPairs $ \(id, val) ->
        ensureExists conn "DESCRIPTOR_SETS"
            ["FK_DESCRIPTOR_ID", "VALUE", "FK_ITEM_ID"]
            [toSql id, toSql $ val, toSql itemID]




setUpAllTables conn =
    forM_ [setUpDescriptors, setUpTrailData, setUpItems, setUpDescriptorSets]
        (\q -> run conn q [])

data DescriptorType = Innate | Mod | Socket | Enchant | RequiredLevel | Level | StatReq
                    | Description | EmptySocket | Name | ItemType deriving (Show, Enum)

-- General ID, with phantom type for not getting them mixed up
newtype ID a = ID Int deriving Show

-- Convertible instances to HDBC knows how to convert my types to DB values and back
instance Convertible (ID a) SqlValue where
    safeConvert (ID val) = Right $ toSql val

instance Convertible SqlValue (ID a) where
    safeConvert = Right . ID . fromSql

instance Convertible DescriptorType SqlValue where
    safeConvert = Right . toSql . fromEnum

instance Convertible SqlValue DescriptorType where
    safeConvert = Right . toEnum . fromSql

instance Convertible Float SqlValue where
    safeConvert = Right . toSql . float2Double

-- Types for each table to use with ID
data Descriptors = Descriptors
data Items = Items
data TrailData = TrailData
data DescriptorSets = DescriptorSets

setUpDescriptors =
    "create table DESCRIPTORS \
    \( ID integer primary key not null \
    \, TYPE integer not null\
    \, EXPRESSION text not null \
    \, unique (type, expression));"

setUpTrailData =
    "create table TRAIL_DATA \
    \( ID integer primary key not null \
    \, DATA blob not null);"

setUpItems =
    "create table ITEMS \
    \( ID integer primary key not null \
    \, RANDOM_ID blob not null \
    \, GUID integer not null \
    \, CONTAINER text not null\
    \, SLOT text not null\
    \, POSITION integer not null\
    \, LEAD_DATA blob not null\
    \, FK_TRAIL_DATA_ID integer not null \
    \, DATE text not null \
    \, foreign key(FK_TRAIL_DATA_ID) references TRAIL_DATA(ID));"

setUpDescriptorSets =
    "create table DESCRIPTOR_SETS \
    \( ID integer primary key not null \
    \, FK_ITEM_ID integer not null \
    \, FK_DESCRIPTOR_ID integer not null \
    \, VALUE text unique not null \
    \, foreign key(FK_ITEM_ID) references ITEMS(ID)\
    \, foreign key(FK_DESCRIPTOR_ID) references DESCRIPTORS(ID));"
