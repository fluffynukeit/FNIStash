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

module FNIStash.Logic.DB (
    initializeDB
) where

import FNIStash.File.Item

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Monoid
import Data.Time.LocalTime

import Filesystem.Path.CurrentOS

initializeDB appRoot = do
    conn <- connectSqlite3 $ encodeString (appRoot </> "fnistash.db")
    run conn ("CREATE TABLE registry " <>
        "( guid INTEGER NOT NULL" <>
        ", random_id BLOB NOT NULL" <>
        ", register_date TEXT NOT NULL" <>
        ", current_location TEXT NOT NULL" <>
        ", description TEXT NOT NULL" <>
        ", comment TEXT NOT NULL" <>
        ", binary_data_ BLOB NOT NULL)") []
    return conn

register env conn item@(Item {..}) = do
    zonedTime <- getZonedTime
    let localTime = zonedTimeToLocalTime zonedTime
    run conn "INSERT INTO registry VALUES (?, ?, ?, ?, ?, ?, ?)"
        [ toSql itemGUID, toSql itemRandomID, toSql localTime, toSql (locToId itemLocation)
        , toSql (showItem item), toSql (""::String), toSql (itemAsBS env item)]
