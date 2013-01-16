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

module FNIStash.File.DAT (
    getDAT,
    textDAT,
    findSection,
    findVar,
    DATVar(..)
) where


-- Based on the DAT2TXT Python program by cienislaw.
import FNIStash.File.General
import FNIStash.File.VarIDs

import Data.Binary.Strict.Get
import qualified Data.Text as T
import Data.List
import Data.Word
import Data.Int
import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe (isJust)

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

-- Show instance for DATs

textDAT :: DATNode -> T.Text
textDAT d = textDATNodeIndexed 0 d where
    textDATNodeIndexed i dn =
        let nodeStart = T.pack "[" <> (lkupVarDes $ datNodeID dn) <> T.pack "] " <> intToHex i
                <> T.pack "\n"
            nodeEnd = T.pack "[/" <> (lkupVarDes $ datNodeID dn) <> T.pack "] " <> intToHex i
                <> T.pack "\n"
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
















