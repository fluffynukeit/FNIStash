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

module FNIStash.Logic.DB where

import FNIStash.File.Item
import FNIStash.Logic.Env

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Monoid
import Data.Time.LocalTime

import Filesystem.Path.CurrentOS
import Filesystem

import Debug.Trace

handleDBError io = catchSql io (\err -> trace ("**SQL ERROR**: " ++ show err) return ())

initializeDB appRoot = do
    let dbPath = appRoot </> "fnistash.db"
    dbExists <- isFile dbPath
    conn <- connectSqlite3 $ encodeString dbPath
    if dbExists then
        return () -- don't need to make a new table
        else
            run conn ("CREATE TABLE registry " <>
                "( guid INTEGER NOT NULL" <>
                ", random_id BLOB PRIMARY KEY NOT NULL" <>
                ", register_date TEXT NOT NULL" <>
                ", current_location TEXT NOT NULL" <>
                ", description TEXT NOT NULL" <>
                ", comment TEXT NOT NULL" <>
                ", binary_data_ BLOB NOT NULL)") [] >> commit conn
    return conn

register env item@(Item {..}) = do
    zonedTime <- getZonedTime
    let localTime = zonedTimeToLocalTime zonedTime
        conn = dbConn env
    wasRegistered <- isRegistered env item
    if wasRegistered then
        return False
        else do
        itemsInserted <- run conn "INSERT INTO registry VALUES (?, ?, ?, ?, ?, ?, ?)"
            [ toSql itemGUID, toSql itemRandomID, toSql localTime, toSql (locToId itemLocation)
            , toSql (showItem item), toSql (""::String), toSql (itemAsBS env item)]
        if itemsInserted == 0 then return False else commit conn >> return True

isRegistered env (Item {..}) = do
    matchingGuys <- quickQuery' (dbConn env) "SELECT random_id FROM registry WHERE random_id = ?" [toSql itemRandomID]
    if length matchingGuys == 0 then return False else return True
