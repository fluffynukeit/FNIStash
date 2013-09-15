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
, rollbackDB
, register
, keywordStatus
, allItemSummaries
, getItemFromDb
, getSharedStashFromDb
, locationChange
, allLocationContents
, allGUIDs
, commitDB
, allDBItems
, ItemClass(..)
, ItemSummary(..)
, ItemMatch(..)
, ItemStatus(..)
, RegisterSummary(..)
)
where

import FNIStash.Logic.Item
import FNIStash.Logic.Env
import FNIStash.File.SharedStash
import FNIStash.File.Variables
import FNIStash.File.General
import FNIStash.File.Item

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
import Data.Binary.Put
import Data.Word

import Filesystem.Path.CurrentOS hiding (FilePath)
import Filesystem

import Debug.Trace

---- Define conversion to/from db

instance Convertible GUID SqlValue where
    safeConvert (GUID{..}) = safeConvert guidVal


-- Record types for returning data from DB

data ItemStatus = Stashed | Inserted | Archived | Elsewhere deriving (Eq, Show, Read, Ord)
data ItemSummary = ItemSummary
    { summaryName :: String
    , summaryDbID :: Int
    , summaryItemClass :: ItemClass
    , summaryStatus :: ItemStatus
    , summaryIcon :: FilePath
    } deriving (Eq, Show, Ord)
data ItemMatch = ItemMatch
    { matchDbID :: Int
    , matchFlag :: Bool
    , matchLocation :: Maybe Location -- No location if item is archived or inserted
    } deriving (Eq, Ord, Show)

data RegisterResult = New
                    | Updated
                    | NoChange
                    deriving Eq

data RegisterSummary = RegisterSummary
    { news :: [Item]
    , updates :: [Item]
    , noChange :: [Item]
    }

-- DB stuff

handleDB = handleSql
commitDB Env{..} = commit dbConn


initializeDB appRoot = do
    let dbPath = appRoot </> "fnistash.db"
    dbExists <- isFile dbPath

    when dbExists $ copyFile dbPath (appRoot </> "fnistash.db_bak")
    
    conn <- connectSqlite3 $ encodeString dbPath

    -- need to enable foreign keys for each connection
    runRaw conn "PRAGMA foreign_keys = ON;"

    when (not dbExists) $ setUpAllTables conn >> commit conn

    return conn

rollbackDB Env{..} = rollback dbConn

-- Given a list of items, registers those that have not been registered yet and
-- returns the newly registered items in a list
register env@Env{..} fVers status items = do
    setStashedToElsewhere env
    registerResults <- forM items $ \item@(Item {..}) -> do
        getReg <- getRegistered env item
        putStrLn $ "Attempting to register " ++ show iName
        case getReg of
            Just id -> do
                dataHasChanged <- dataChange env id item
                case dataHasChanged of
                    True  -> do     -- Item data has changed.  Remove and re-insert
                        deleteID env id
                        addItemToDB env fVers status item
                        return $ (Updated, item)
                    False -> do -- Item data the same.  Only update location.
                        updateLocation env id iLocation status
                        return $ (NoChange, item)
            Nothing -> do -- Item not previously registered
                addItemToDB env fVers status item
                return $ (New, item)

    -- now clean up any trail_data entries that are irrelevant.
    cleanUpDB env
    let news = map snd $ filter ((== New) . fst) registerResults
        upda = map snd $ filter ((== Updated) . fst) registerResults
        noch = map snd $ filter ((== NoChange) . fst) registerResults
    return $ RegisterSummary news upda noch


setStashedToElsewhere Env{..} = do
    let query = "update ITEMS set STATUS=? where STATUS=?;"
    void $ run dbConn query [toSql $ show Elsewhere, toSql Stashed]


cleanUpDB Env{..} = do
    let query = "delete from TRAIL_DATA where ID not in (select FK_TRAIL_DATA_ID from ITEMS);"
    void $ run dbConn query []

deleteID Env{..} iD = do
    let query = "delete from ITEMS where ID = ?;"
    void $ run dbConn query [toSql iD]

dataChange env id item = do
    (Right (Just oldData)) <- getItemFromDb env (Archive id) -- dangerous pattern matching...
    return $ iPartition oldData /= iPartition item

updateLocation Env{..} id Location{..} status = do
    void $ run dbConn updateLocStatById [toSql locContainer, toSql locIndex, toSql status, toSql id]

updateLocation Env{..} id InsertedInSocket _ = do
    void $ run dbConn updateLocStatById [ toSql ("SHARED_STASH_BAG_ARMS"::String)
                                 , toSql (0::Int)
                                 , toSql Inserted
                                 , toSql id]


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
allItemSummaries (env@(Env{..})) = do
    let query = "select NAME, CONTAINER, STATUS, ID, GUID from ITEMS order by NAME;"
        makeSumm row = ItemSummary  (fromSql $ row !! 0)
                                    (fromSql $ row !! 3)
                                    (contToClass (fromSql $ row !! 1 :: String))
                                    (read . fromSql $ row !! 2)
                                    (getImage $ GUID $ fromSql $ row !! 4)
        getImage guid = fromJust (lkupItemGUID guid >>= searchAncestryFor env vICON)
       
    itemData <- quickQuery' dbConn query []
    return $ map makeSumm itemData

allLocationContents (env@Env{..}) = allItemsSatisfying env " where STATUS=?" [toSql Stashed]

contToClass "SHARED_STASH_BAG_ARMS" = Arms
contToClass "SHARED_STASH_BAG_CONSUMABLES" = Consumables
contToClass "SHARED_STASH_BAG_SPELLS" = Spells

allDBItems env = do
    allItemResults <- allItemsSatisfying env " where STATUS<>?" [toSql Inserted] -- returns all items in DB (not inserted)
    return $ flip map allItemResults $ \r -> r >>= return . snd -- toss out the location data but keep errors

allItemsSatisfying env@Env{..} whereClause params = do
    let query = getItemDataQuery ++ whereClause
    rows <- quickQuery' dbConn query params
    return $ flip map rows $ \(dbID:lead:trail:cont:slot:pos:name:_) ->
        let bs = buildItemBytes env lead trail cont slot pos
        in case fst $ runGet (getItem env (Just $ fromSql dbID) bs) bs of
            Left err -> Left $ "Item named " ++ fromSql name ++
                " failed binary parsing with error: " ++ err
            Right item -> Right (iLocation item, Just item)



allGUIDs :: Env -> IO [GUID]
allGUIDs Env{..} =
    let query = "select distinct GUID from ITEMS order by GUID asc;"
    in do
        rows <- quickQuery' dbConn query []
        return $ map mkGUID rows
        where mkGUID row = GUID . fromSql $ row !! 0

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

getRegistered env (Item {..}) = do
    matchingGuys <- quickQuery' (dbConn env) "select ID from ITEMS where RANDOM_ID = ?" [toSql iRandomID]
    if length matchingGuys == 0 then return Nothing else return $ Just (fromSql $ head matchingGuys !! 0)

addItemToDB env fVers status item@(Item {..}) = do
    -- First register the item data, starting with the Trail data
    trailID <- insertTrailData env item
    itemID <- insertItem env fVers item trailID status
    -- First collect all the descriptors for the item
    let descriptorList = allDescriptorsOf item
    
    descListWithValue <- forM descriptorList $ insertDescriptor env  

    -- Then insert them into the db
    insertDescriptorSet env itemID descListWithValue

    -- Finally, insert any socketed gems into the DB as well
    forM_ (iGemsAsItems) (addItemToDB env fVers status) 


getItemFromDb :: Env -> Location -> IO (Either String (Maybe Item))
getItemFromDb (env@Env{..}) loc = do
    let (query, params) = case loc of
            Archive id     -> (getItemDataQueryID, [toSql id])
            Location c _ i -> (getItemDataQueryLoc, [toSql c, toSql i, toSql Stashed])
    qResults <- quickQuery' dbConn query params
    return $ case qResults of
        []    -> Right Nothing -- no rows returns
        [row] -> Just <$> case parseItemRow env row of
            Left (s, bs) -> Left s
            Right item ->   Right item

getSharedStashFromDb :: Env -> IO SharedStash
getSharedStashFromDb env@Env{..} = do
    let query = getItemDataQuery ++ " where STATUS = ?"
    rows <- quickQuery' dbConn query [toSql Stashed]
    return $ map (parseItemRow env) rows


parseItemRow env@Env{..} (dbID:lead:trail:c:s:p:name:_) =
    let
        bs = buildItemBytes env lead trail c s p
    in case fst $ runGet (getItem env (Just $ fromSql dbID) bs) bs of
        Left err -> Left $ ("Item with name '" ++ show (fromSql name::String) ++ 
            "' was found in DB but binary parsing failed with error: " ++ err, bs)
        Right item -> Right (item)

buildItemBytes env lead trail c s p =
    let loc = Location (fromSql c) (fromSql s) (fromSql p)
        part = Partition (fromSql lead) (fromSql trail)
    in BS.drop 4 . toStrict . runPut $ putPartition (encodeLocationBytes env loc) part


-- Location argument order is From -> To
locationChange :: Env -> Location -> Location -> IO (Either String [(Location, Maybe Item)])
-- First handle case where we are moving an item from archive to a stash location, which might have
-- an item there already
locationChange (env@Env{..}) (arch@(Archive id)) (loc@Location{..}) = do
    -- For any item at the destination and stashed, mark it as Archived
    updates1 <- locationChange env loc arch
    -- Now set new location data
    updateLocation env id loc Stashed
    item <- getItemFromDb env loc
    return $ updates1 >>= const item >>= \i -> (++) <$> updates1 <*> Right [(loc, i), (arch, Nothing)]


-- The case where we are moving from stash to archive
locationChange (env@Env{..}) (loc@Location{..}) (arch@Archive{..}) = do
    item <- getItemFromDb env loc -- get the info for the item we are moving (if it exists at loc)
    rows <- run dbConn queryToArchive [toSql Archived, toSql locContainer, toSql locIndex, toSql Stashed]
    let result = item >>= \i ->
            return $ (++) [(loc, Nothing)] (if rows > 0 then [(Archive (fromJust . iID $ fromJust i), i)] else [])
    return $ result

locationChange (env@Env{..}) locFrom@(Location _ _ _) locTo@(Location _ _ _) = do
    let selQuery = "select ID from ITEMS " ++ whereContPosStat
        upQuery = "update ITEMS set CONTAINER=?, POSITION=?, STATUS=?"
        contPosStatVals loc = [toSql $ locContainer loc, toSql $ locIndex loc, toSql Stashed]
        getID r = fromSql $ ((head r) !! 0)
    -- First check to see if anything is already at the TO location
    rows <- quickQuery' dbConn selQuery (contPosStatVals locTo)
    let destOccupied = length rows > 0

    -- Update the item that will be in the new TO location
    run dbConn (upQuery ++ whereContPosStat) $
        contPosStatVals locTo ++ contPosStatVals locFrom

    -- If something exists at the TO location, swap it with the FROM location
    when destOccupied $ do
        void $ run dbConn (upQuery ++ " where ID=?;") ((contPosStatVals locFrom) ++ [getID rows])

    itemF <- getItemFromDb env locFrom
    itemT <- getItemFromDb env locTo
    return $ sequence [itemF, itemT] >>= \(f:t:[]) -> Right [(locFrom, f), (locTo, t)]

locationChange _ (Archive _) (Archive _) = return $ Right []

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

insertItem :: Env -> Word32 -> Item -> ID TrailData -> ItemStatus -> IO (ID Items)
insertItem (Env {..}) fVers item@(Item {..}) trailDataID statusIn = do
    zonedTime <- getZonedTime
    let localTime = zonedTimeToLocalTime zonedTime
        (container, slot, position, status) = case iLocation of
            Location a b c   -> (a, b, c, statusIn)
            InsertedInSocket -> ("SHARED_STASH_BAG_ARMS", "BAG_ARMS_SLOT", 0, Inserted) -- gems always go in Arms

    ensureExists dbConn "ITEMS" [ ("RANDOM_ID", toSql iRandomID)
                                , ("GUID", toSql $ iBaseGUID iBase)
                                , ("FILE_VERSION", toSql fVers)
                                , ("NAME", toSql $ descriptorString iName)
                                , ("CONTAINER", toSql container)
                                , ("SLOT", toSql slot)
                                , ("POSITION", toSql position)
                                , ("STATUS", toSql status)
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
        (\q -> runRaw conn q )


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

instance Convertible ItemStatus SqlValue where
    safeConvert = Right . toSql . show

instance Convertible SqlValue ItemStatus where
    safeConvert = Right . read . fromSql 

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
    \, FILE_VERSION integer not null \
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
    \, foreign key(FK_ITEM_ID) references ITEMS(ID) on delete cascade \
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

getItemDataQuery = "select i.ID, LEAD_DATA, DATA, CONTAINER, SLOT, POSITION, NAME \
    \ from ITEMS i \
    \ inner join TRAIL_DATA t on i.FK_TRAIL_DATA_ID = t.ID "
    
getItemDataQueryID = getItemDataQuery ++
    " where i.ID = (?);"

getItemDataQueryLoc = getItemDataQuery ++ whereContPosStat

updateLocStatById  = "update ITEMS set CONTAINER=?, POSITION=?, STATUS=? \
                \where ID = (?);"
queryToArchive = "update ITEMS set STATUS=? " ++ whereContPosStat

whereContPosStat = " where CONTAINER=? and POSITION=? and STATUS=?;"

