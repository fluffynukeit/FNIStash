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
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module FNIStash.Logic.DB
( handleDB
, initializeDB
, register
, keywordStatus
, allItemSummaries
, getItemFromDb
, ItemClass(..)
, ItemSummary(..)
, ItemMatch(..)
)
where

import FNIStash.Logic.Item
import FNIStash.Logic.Env
import FNIStash.File.Variables
import FNIStash.File.General

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Monoid
import Data.Time.LocalTime
import Data.Maybe
import Control.Monad
import Control.Applicative ((<$>), (<*>))
import qualified Data.ByteString as BS
import qualified Data.List as L
import qualified Data.Text as T
import Data.Convertible.Base
import GHC.Float
import Text.Parsec
import Data.Binary.Strict.Get

import Filesystem.Path.CurrentOS
import Filesystem

import Debug.Trace

---- Define conversion to/from db

instance Convertible GUID SqlValue where
    safeConvert (GUID{..}) = safeConvert guidVal


-- Record types for returning data from DB

data ItemStatus = Stashed | Inserted | Archived deriving (Eq, Show, Read, Ord)
data ItemSummary = ItemSummary
    { summaryName :: String
    , summaryDbID :: Int
    , summaryItemClass :: ItemClass
    , summaryStatus :: ItemStatus
    } deriving (Eq, Show, Ord)
data ItemMatch = ItemMatch
    { matchDbID :: Int
    , matchFlag :: Bool
    , matchLocation :: Maybe Location -- No location if item is archived or inserted
    } deriving (Eq, Ord, Show)

-- DB stuff

handleDB = handleSql

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
    succItems <- fmap catMaybes $ withTransaction conn $ const $ forM items $ \item@(Item {..}) -> do
        wasRegistered <- isRegistered env item

        if wasRegistered then
            return Nothing
            else do
            addItemToDB env item
            return $ Just item
            
    return succItems

keywordStatus :: Env -> String -> IO (Either String [ItemMatch])
keywordStatus env (parseKeywords -> Left error) = return . Left . show $ error
keywordStatus env (parseKeywords -> Right keywordList) = do
    let keywords = if length keywordList > 0 then keywordList else [""]
        conn = dbConn env
        keywordExpr = L.intercalate " and " $ replicate (length keywords) "FD like ?"
        query = (keywordQuery keywordExpr)
        s word = toSql $ "%" ++ word ++ "%"
    qResults <- quickQuery' conn query $ map s keywords
    let buildMatch row =
            let dbID = fromSql $ row !! 3
                status = read . fromSql $ row !! 4
                matchFlag = fromSql $ row !! 5
                mLoc = case status of
                    Archived -> Nothing
                    Inserted -> Nothing
                    _        -> Just $ Location (fromSql $ row !! 0) (fromSql $ row !! 1) (fromSql $ row !! 2)
            in ItemMatch dbID matchFlag mLoc

    return . Right $ map buildMatch qResults

allItemSummaries :: Env -> IO [ItemSummary]
allItemSummaries (dbConn -> conn) = do
    let query = "select NAME, CONTAINER, STATUS, ID from ITEMS order by NAME;"
        makeSumm row = ItemSummary  (fromSql $ row !! 0)
                                    (fromSql $ row !! 3)
                                    (contToClass (fromSql $ row !! 1 :: String))
                                    (read . fromSql $ row !! 2)
    itemData <- quickQuery' conn query []
    return $ map makeSumm itemData

contToClass "SHARED_STASH_BAG_ARMS" = Arms
contToClass "SHARED_STASH_BAG_CONSUMABLES" = Consumables
contToClass "SHARED_STASH_BAG_SPELLS" = Spells

parseKeywords :: String -> Either ParseError [String]
parseKeywords (deblank -> s) = parse keywordParser "" s

deblank = T.unpack . T.strip . T.pack

keywordParser :: (Stream s m Char) => ParsecT s u m [String]
keywordParser = do
    words <- sepBy (quoted <|> normalWord) spaces
    return words

quoted :: (Stream s m Char) => ParsecT s u m String
quoted = do
    char '"'
    content <- many (noneOf "\"")
    char '"' <?> "closing quote."
    return content

normalWord :: (Stream s m Char) => ParsecT s u m String
normalWord = many1 (noneOf " ")

isRegistered env (Item {..}) = do
    matchingGuys <- quickQuery' (dbConn env) "select RANDOM_ID from ITEMS where RANDOM_ID = ?" [toSql iRandomID]
    if length matchingGuys == 0 then return False else return True

addItemToDB env item@(Item {..}) = do
    -- First register the item data, starting with the Trail data
    trailID <- insertTrailData env item
    itemID <- insertItem env item trailID
    -- First collect all the descriptors for the item
    let descriptorList = allDescriptorsOf item
    
    descListWithValue <- forM descriptorList $ insertDescriptor env  

    -- Then insert them into the db
    insertDescriptorSet env itemID descListWithValue

    -- Finally, insert any socketed gems into the DB as well
    forM_ (iGemsAsItems) (addItemToDB env) 


getItemFromDb :: Env -> Int -> IO (Either String Item)
getItemFromDb (env@Env{..}) id = do
    qResults <- quickQuery' dbConn getItemDataQuery [toSql id]
    return $ case qResults of
        []    -> Left $ "No item with ID " ++ show id ++ " was found in DB."
        ((lead:loc:trail:_):_) ->
            let bs = fromSql lead `BS.append` fromSql loc `BS.append` fromSql trail
            in case fst $ runGet (getItem env bs) bs of
                Left err -> Left $ "Item with ID " ++ show id ++
                    " was found in DB but binary parsing failed with error: " ++ err
                Right item -> Right item
    

-- Use like this: ensureExists conn "ITEMS" ["RANDOM_ID", "GUID"] [toSql itemRandomID, toSql itemGUID]
-- Tries to do an insert and ignores if constraint is broken.  Returns the ID of the row with
-- head of cols matching head of values
ensureExists conn table colVals = do
    let cols = map fst colVals
        vals = map snd colVals
        colsTuple = stringTuple cols
        marksTuple = stringTuple $ replicate (length vals) "?"
        insertQ = "insert or ignore into " <> table <> colsTuple <> " values " <> marksTuple
        selectQ = "select ID from " <> table <> " where " <> (head cols) <> " = (?)"
    --traceShow (insertQ, selectQ, cols, vals) $ return ()
    run conn insertQ vals
    idList <- quickQuery' conn selectQ [head vals]
    --traceShow (L.concat idList) $ return ()
    (return $ fromSql $ (head . L.concat) idList) :: IO (ID a)

stringTuple strings = "(" <> (L.intercalate "," strings) <> ")"

insertTrailData :: Env -> Item -> IO (ID TrailData)
insertTrailData (Env {..}) item@(Item {..}) =
    ensureExists dbConn "TRAIL_DATA" [("DATA", toSql $ pAfterLocation iPartition)]

insertItem :: Env -> Item -> ID TrailData -> IO (ID Items)
insertItem (Env {..}) item@(Item {..}) trailDataID = do
    zonedTime <- getZonedTime
    let localTime = zonedTimeToLocalTime zonedTime
        (container, slot, position, status) = case iLocation of
            Location a b c   -> (a,b, show c, Stashed)
            InsertedInSocket -> ("SHARED_STASH_BAG_ARMS", "", "", Inserted) -- gems always go in Arms

    ensureExists dbConn "ITEMS" [ ("RANDOM_ID", toSql iRandomID)
                                , ("GUID", toSql $ iBaseGUID iBase)
                                , ("NAME", toSql $ descriptorString iName)
                                , ("CONTAINER", toSql container)
                                , ("SLOT", toSql slot)
                                , ("POSITION", toSql position)
                                , ("STATUS", toSql $ show status)
                                , ("LEAD_DATA", toSql $ pBeforeLocation iPartition)
                                , ("FK_TRAIL_DATA_ID", toSql trailDataID)
                                , ("DATE", toSql localTime)]

insertDescriptor :: Env -> Descriptor -> IO (ID Descriptors, String)
insertDescriptor (Env {..}) (Descriptor desc val _) = do
    id <- ensureExists dbConn "DESCRIPTORS" [("EXPRESSION", toSql $ (desc::String))]
    return (id, show val)

insertDescriptorSet :: Env -> ID Items -> [(ID Descriptors, String)] -> IO ()
insertDescriptorSet (Env {..}) itemID descIDValPairs =
    forM_ descIDValPairs $ \(id, val) ->
        ensureExists dbConn "DESCRIPTOR_SETS"
            [ ("FK_DESCRIPTOR_ID", toSql id)
            , ("VALUE", toSql $ val)
            , ("FK_ITEM_ID", toSql itemID)]

setUpAllTables conn =
    forM_ [setUpDescriptors, setUpTrailData, setUpItems, setUpDescriptorSets]
        (\q -> run conn q [])


data DescriptorType = Innate | Effect | Socket | Enchant | RequiredLevel | Level | StatReq
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
    \, EXPRESSION text not null \
    \, unique (expression));"

setUpTrailData =
    "create table TRAIL_DATA \
    \( ID integer primary key not null \
    \, DATA blob not null);"

setUpItems =
    "create table ITEMS \
    \( ID integer primary key not null \
    \, RANDOM_ID blob not null \
    \, GUID integer not null \
    \, NAME text not null \
    \, CONTAINER text not null\
    \, SLOT text not null\
    \, POSITION text not null\
    \, STATUS text not null\
    \, LEAD_DATA blob not null\
    \, FK_TRAIL_DATA_ID integer not null \
    \, DATE text not null \
    \, foreign key(FK_TRAIL_DATA_ID) references TRAIL_DATA(ID));"

setUpDescriptorSets =
    "create table DESCRIPTOR_SETS \
    \( ID integer primary key not null \
    \, FK_ITEM_ID integer not null \
    \, FK_DESCRIPTOR_ID integer not null \
    \, VALUE real not null \
    \, foreign key(FK_ITEM_ID) references ITEMS(ID)\
    \, foreign key(FK_DESCRIPTOR_ID) references DESCRIPTORS(ID));"

keywordQuery condString =
    "select CONTAINER, SLOT, POSITION, ID, STATUS, " ++ condString ++ " \
    \ from ( \
        \ select i.CONTAINER, i.SLOT, i.POSITION, i.ID, i.STATUS, \
        \ i.NAME || char(10) || group_concat(replace(d.EXPRESSION, '[*]', s.VALUE), char(10)) as FD \
        \ from DESCRIPTORS d \
        \ inner join DESCRIPTOR_SETS s \
        \ on s.FK_DESCRIPTOR_ID = d.ID \
        \ inner join ITEMS i \
        \ on s.FK_ITEM_ID = i.ID \
        \ group by s.FK_ITEM_ID \
    \ );"

getItemDataQuery =
    "select LEAD_DATA, X'FFFFFFFF', DATA \
    \ from ITEMS i \
    \ inner join TRAIL_DATA t on i.FK_TRAIL_DATA_ID = t.ID \
    \ where i.ID = (?);"
