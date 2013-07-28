-----------------------------------------------------------------------------
--
-- Module      :  FNIStash.File.DAT
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

module FNIStash.File.DAT
    ( getDAT
    , searchNodeTree
    , searchNodeTreeWith
    , lkupVar
    , subNodeAt
    , varAt
    , intVar, floatVar, doubleVar, word32Var, textVar, boolVar, int64Var, stringVar
    , readDATFiles
    , lkupDATFile
    , DATNode(..)
    , VarID
    , DATFiles
    ) where

-- This file defines parsing functions for reading TL2's DAT file format.  One thing that might
-- be confusing is that VarIDs are used to identify both individual variables and the nodes that
-- contain them


-- Based on the DAT2TXT Python program by cienislaw.
import FNIStash.File.General
import FNIStash.File.PAK

import qualified Data.Binary.Strict.Get as SG
import qualified Data.Text as T
import qualified Data.ByteString as SBS
import qualified Data.Map as M
import Data.List
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Word
import Data.Monoid
import Data.Int



-- Functions for searching DAT records

-- Deep searches a node tree for a node matching VarID
searchNodeTree :: VarID -> DATNode -> Maybe DATNode
searchNodeTree v d = searchNodeTreeWith (\x -> datNodeID x == v) d

searchNodeTreeWith :: (DATNode -> Bool) -> DATNode -> Maybe DATNode
searchNodeTreeWith pred d = searchNodeList [d] pred

-- Helper: Deep searches a list of nodes for a node satisfying the given predicate
searchNodeList :: [DATNode] -> (DATNode -> Bool) -> Maybe DATNode
searchNodeList [] _ = Nothing
searchNodeList (x:xs) pred
    | pred x = Just x
    | isJust (searchNodeList xs pred) = searchNodeList xs pred
    | otherwise = searchNodeList (datSubNodes x) pred

-- Extract the variable with the given VarID from the given DATNode
lkupVar :: VarID -> DATNode -> Maybe DATVar
lkupVar v d = snd <$> (find (\(id, var) -> id == v) $ datNodeVars d)

-- Return a node based on location in children of given DATNode
subNodeAt :: Word32 -> DATNode -> Maybe DATNode
subNodeAt i d =
    if i < fromIntegral (length $ datSubNodes d)
    then Just $ datSubNodes d !! fromIntegral i else Nothing


varAt :: Word32 -> DATNode -> Maybe DATVar
varAt i (DATNode{..}) =
    if i < fromIntegral (length datNodeVars)
    then Just $ snd $ datNodeVars !! fromIntegral i else Nothing

-- Conversion of DAT types to normal types

intVar (DATInt i) = Just i
intVar _ = Nothing

floatVar (DATFloat i) = Just i
floatVar _ = Nothing

doubleVar (DATDouble i) = Just i
doubleVar _ = Nothing

word32Var (DATWord i) = Just i
word32Var _ = Nothing

textVar (DATText i) = Just i
textVar (DATTranslate i) = Just i
textVar _ = Nothing

stringVar (DATText i) = Just $ T.unpack i
stringVar (DATTranslate i) = Just $ T.unpack i
stringVar _ = Nothing

boolVar (DATBool i) = Just i
boolVar _ = Nothing

int64Var (DATInt64 i) = Just i
int64Var _ = Nothing


-- Data declarations

type TextID = Word32
type VarID = Word32


type DATDict = [(TextID, T.Text)]
type DATVars = [(VarID, DATVar)]


data DATNode = DATNode {
    datNodeID :: VarID,
    datNodeVars :: DATVars,
    datSubNodes :: [DATNode]
    }
    deriving (Show, Eq)

data DATVar =
    DATInt Int |
    DATFloat Float |
    DATDouble Double |
    DATWord Word32 |
    DATText T.Text |
    DATBool Bool |
    DATInt64 Int64 |
    DATTranslate T.Text
    deriving (Show, Eq, Ord)

-- Get functions

getDAT :: SG.Get DATNode
getDAT = do
    vers <- SG.getWord32le
    dict <- getDATDict
    root <- getDATNode dict
    return root

getDATNode :: DATDict -> SG.Get DATNode
getDATNode dict = DATNode <$> SG.getWord32le <*> getDATVars dict <*>
    (SG.getWord32le >>= (\x -> replicateM (fromIntegral x) (getDATNode dict)))

getDATDict :: SG.Get DATDict
getDATDict = do
    numEntries <- SG.getWord32le
    tuplesList <- replicateM (fromIntegral numEntries) $ (,) <$> SG.getWord32le <*> getTorchText
    return tuplesList

getDATVars :: DATDict -> SG.Get DATVars
getDATVars dict = do
    numEntries <- SG.getWord32le
    replicateM (fromIntegral numEntries) (getDATVar dict)

getDATVar :: DATDict -> SG.Get (VarID, DATVar)
getDATVar dict = do
    varID <- SG.getWord32le
    varType <- SG.getWord32le
    varVal <- case varType of
        1 -> DATInt <$> (SG.getWord32le >>= return . fromIntegral)
        2 -> DATFloat <$> (SG.getWord32le >>= return . wordToFloat)
        3 -> DATDouble <$> (SG.getWord64le >>= return. wordToDouble)
        4 -> DATWord <$> SG.getWord32le
        5 -> DATText <$> (SG.getWord32le >>= return . (\x -> maybe T.empty id (lookup x dict)))
        6 -> DATBool <$> (SG.getWord32le >>= return . (\x -> if x /= 0 then True else False))
        7 -> DATInt64 <$> (SG.getWord64le >>= return . fromIntegral)
        8 -> DATTranslate <$> (SG.getWord32le >>= return . (\x -> maybe T.empty id (lookup x dict)))
    return (varID, varVal)

-- "Show" functions for DATs -- commented out because no longer needed after getting them working

--textDAT :: DATNode -> T.Text
--textDAT d = textDATNodeIndexed 0 d where
--    textDATNodeIndexed i dn =
--        let nodeStart = "[" <> (lkupVarDes $ datNodeID dn) <> "] " <> T.pack (intToHex i)
--                <> "\n"
--            nodeEnd = "[/" <> (lkupVarDes $ datNodeID dn) <> "] " <> T.pack (intToHex i)
--                <> "\n"
--            nodeVars = textVarList $ datNodeVars dn
--            indexedSubNodes = zip [0..] $ datSubNodes dn
--            nodeMid = mconcat $ map (\(i,n) -> textDATNodeIndexed i n) indexedSubNodes
--        in nodeStart <> nodeVars <> nodeMid <> nodeEnd
--
--textVarList :: DATVars -> T.Text
--textVarList vars = foldl' (\acc pair -> acc <> (textVarPair pair)) T.empty vars
--
--textVarPair :: (VarID, DATVar) -> T.Text
--textVarPair (v,d) =
--    let f n p = (show n) ++ " (" ++ p ++ ")\n"
--    in lkupVarDes v <> (T.pack (" : " ++ (case d of
--        DATInt i -> f i "Int"
--        DATFloat i -> f i "Float"
--        DATDouble i -> f i "Double"
--        DATWord i -> f i "Word"
--        DATText i -> f i "Text"
--        DATBool i -> f i "Bool"
--        DATInt64 i -> f i "Int64"
--        DATTranslate i -> f i "Translate"
--        )))


-- functions for building DAT maps
type DATFiles a = M.Map a  DATNode

-- Creates a new map (of type DATFiles a).  The new map is a mapping between some
-- key of type a and it's associated DAT file.  So for instance, we can look the DAT
-- file for an item based on the GUID contained in that DAT file.  The text parameter
-- is used to restrict the path so that only DAT files are encountered.
readDATFiles :: Ord a => PAKFiles -> T.Text -> (DATNode -> a) -> DATFiles a
readDATFiles pak pathSubStr keyFxn =
    let matchingKeyMap = pakWithKeysContaining pathSubStr pak
        newPair (oldKey,_) =
            let newVal = runGetSuppress getDAT $ maybe SBS.empty id $ lkupPAKFile oldKey pak
                newKey = keyFxn newVal
            in (newKey, newVal)
        datExt (oldKey, _) = T.isInfixOf ".DAT" oldKey
    in M.fromList $ map newPair $ filter datExt $ M.toList matchingKeyMap

lkupDATFile :: Ord a => DATFiles a -> a -> Maybe DATNode
lkupDATFile d k = M.lookup k d









