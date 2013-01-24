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

module FNIStash.File.DAT (
    getDAT,
    textDAT,
    findSection,
    findVar,
    sectionAt,
    intVar, floatVar, doubleVar, word32Var, textVar, boolVar, int64Var, translateVar,
    readDATFiles,
    lkupDATFile,
    DATNode
) where


-- Based on the DAT2TXT Python program by cienislaw.
import FNIStash.File.General
import FNIStash.File.VarIDs
import FNIStash.File.PAK

import Data.Binary.Get
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as M
import Data.List
import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Word
import Data.Monoid
import Data.Int

-- Functions for searching DAT records

findSection :: VarID -> DATNode -> Maybe DATNode
findSection v d = searchNodeList [d] v

searchNodeList :: [DATNode] -> VarID -> Maybe DATNode
searchNodeList [] _ = Nothing
searchNodeList (x:xs) v
    | datNodeID x == v = Just x
    | isJust (searchNodeList xs v) = searchNodeList xs v
    | otherwise = searchNodeList (datSubNodes x) v

findVar :: VarID -> DATNode -> Maybe DATVar
findVar v d = snd <$> (find (\(id, var) -> id == v) $ datNodeVars d)

sectionAt :: Word32 -> DATNode -> Maybe DATNode
sectionAt i d =
    if i < fromIntegral (length $ datSubNodes d)
    then Just $ datSubNodes d !! fromIntegral i else Nothing

intVar (DATInt i) = Just i
intVar _ = Nothing

floatVar (DATFloat i) = Just i
floatVar _ = Nothing

doubleVar (DATDouble i) = Just i
doubleVar _ = Nothing

word32Var (DATWord i) = Just i
word32Var _ = Nothing

textVar (DATText i) = Just i
textVar _ = Nothing

boolVar (DATBool i) = Just i
boolVar _ = Nothing

int64Var (DATInt64 i) = Just i
int64Var _ = Nothing

translateVar (DATTranslate i) = Just i
translateVar _ = Nothing

-- Data declarations

type TextID = Word32

type DATDict = [(TextID, T.Text)]
type DATVars = [(VarID, DATVar)]


data DATNode = DATNode {
    datNodeID :: VarID,
    datNodeVars :: DATVars,
    datSubNodes :: [DATNode]
    }

data DATVar =
    DATInt Int |
    DATFloat Float |
    DATDouble Double |
    DATWord Word32 |
    DATText T.Text |
    DATBool Bool |
    DATInt64 Int64 |
    DATTranslate T.Text


-- Get functions

getDAT :: Get DATNode
getDAT = do
    vers <- getWord32le
    dict <- getDATDict
    root <- getDATNode dict
    return root

getDATNode :: DATDict -> Get DATNode
getDATNode dict = DATNode <$> getWord32le <*> getDATVars dict <*>
    (getWord32le >>= (\x -> replicateM (fromIntegral x) (getDATNode dict)))

getDATDict :: Get DATDict
getDATDict = do
    numEntries <- getWord32le
    tuplesList <- replicateM (fromIntegral numEntries) $ (,) <$> getWord32le <*> getTorchText
    return tuplesList

getDATVars :: DATDict -> Get DATVars
getDATVars dict = do
    numEntries <- getWord32le
    replicateM (fromIntegral numEntries) (getDATVar dict)

getDATVar :: DATDict -> Get (VarID, DATVar)
getDATVar dict = do
    varID <- getWord32le
    varType <- getWord32le
    varVal <- case varType of
        1 -> DATInt <$> (getWord32le >>= return . fromIntegral)
        2 -> DATFloat <$> (getWord32le >>= return . wordToFloat)
        3 -> DATDouble <$> (getWord64le >>= return. wordToDouble)
        4 -> DATWord <$> getWord32le
        5 -> DATText <$> (getWord32le >>= return . (\x -> maybe T.empty id (lookup x dict)))
        6 -> DATBool <$> (getWord32le >>= return . (\x -> if x /= 0 then True else False))
        7 -> DATInt64 <$> (getWord64le >>= return . fromIntegral)
        8 -> DATTranslate <$> (getWord32le >>= return . (\x -> maybe T.empty id (lookup x dict)))
    return (varID, varVal)

-- "Show" functions for DATs

textDAT :: DATNode -> T.Text
textDAT d = textDATNodeIndexed 0 d where
    textDATNodeIndexed i dn =
        let nodeStart = "[" <> (lkupVarDes $ datNodeID dn) <> "] " <> intToHex i
                <> "\n"
            nodeEnd = "[/" <> (lkupVarDes $ datNodeID dn) <> "] " <> intToHex i
                <> "\n"
            nodeVars = textVarList $ datNodeVars dn
            indexedSubNodes = zip [0..] $ datSubNodes dn
            nodeMid = mconcat $ map (\(i,n) -> textDATNodeIndexed i n) indexedSubNodes
        in nodeStart <> nodeVars <> nodeMid <> nodeEnd

textVarList :: DATVars -> T.Text
textVarList vars = foldl' (\acc pair -> acc <> (textVarPair pair)) T.empty vars

textVarPair :: (VarID, DATVar) -> T.Text
textVarPair (v,d) =
    let f n p = (show n) ++ " (" ++ p ++ ")\n"
    in lkupVarDes v <> (T.pack (" : " ++ (case d of
        DATInt i -> f i "Int"
        DATFloat i -> f i "Float"
        DATDouble i -> f i "Double"
        DATWord i -> f i "Word"
        DATText i -> f i "Text"
        DATBool i -> f i "Bool"
        DATInt64 i -> f i "Int64"
        DATTranslate i -> f i "Translate"
        )))


-- functions for building DAT maps
type DATFiles a = M.Map a  DATNode

readDATFiles :: Ord a => PAKFiles -> T.Text -> (DATNode -> a) -> DATFiles a
readDATFiles pak prefix keyFxn =
    let prefixMap = M.filterWithKey (\key _ -> T.isPrefixOf prefix key) pak
        pairList = M.toList prefixMap
        newPair (k1,entry) =
            let Right newVal = runGetWithFail ("Problem reading DAT file " <> k1) getDAT (entryData entry)
                newKey = keyFxn newVal
            in (newKey, newVal)
        newPairList = map newPair pairList
    in M.fromList newPairList



lkupDATFile :: Ord a => DATFiles a -> a -> Maybe DATNode
lkupDATFile d k = M.lookup k d









