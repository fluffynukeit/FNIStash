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

module FNIStash.Logic.DB (
    initializeDB
) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.Monoid

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
        ", binary_data BLOB NOT NULL)") []

