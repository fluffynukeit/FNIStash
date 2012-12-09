-----------------------------------------------------------------------------
--
-- Module      :  FNI.Logic.File
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module FNIStash.Logic.File (
    getScrambled,
    descrambleGameFile,
    scrambleGameFile,
    fileGameData,
    unScrambled,
    unDescrambled,
    bsToItems
) where

import FNIStash.Logic.Crypto
import FNIStash.Logic.Data


import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Either
import Data.Text.Encoding
import Data.Binary.Strict.Get
import Data.Word
import Control.Applicative
import Control.Error (fmapR)
import Data.Binary.Put (putWord32le, runPut)
import Data.ByteString.Lazy (toStrict)
import Debug.Trace (trace)


data GameFile = GameFile {
    fileVersion   :: Word32,
    fileDummy     :: Word8,
    fileChecksum  :: Word32,
    fileGameData  :: BS.ByteString,
    fileSize      :: Word32
    }

newtype Scrambled = Scrambled GameFile
newtype Descrambled = Descrambled GameFile


unScrambled (Scrambled gf) = gf
unDescrambled (Descrambled gf) = gf

getScrambled :: Get Scrambled
getScrambled = do
    version <- getWord32le
    dummy   <- getWord8
    checkSumVal <- case version of
        _ -> getWord32le
    remainingBytes <- remaining
    gameBS  <- getByteString $ remainingBytes - 4
    fsize   <- getWord32le
    return $ Scrambled $ GameFile version dummy checkSumVal gameBS fsize

descrambleGameFile :: Scrambled -> Descrambled
descrambleGameFile (Scrambled scrFile) = Descrambled $ GameFile
    (fileVersion scrFile)
    (fileDummy scrFile)
    (0x00::Word32)
    (descramble $ fileGameData scrFile)
    (fileSize scrFile)

scrambleGameFile :: Descrambled -> Scrambled
scrambleGameFile (Descrambled desFile) = Scrambled $ GameFile
    (fileVersion desFile)
    (fileDummy desFile)
    (checksum $ fileGameData desFile)
    (scramble $ fileGameData desFile)
    (4 + 1 + 4 + 4 + fromIntegral (BS.length $ fileGameData desFile))


getBSPiece :: Get BS.ByteString
getBSPiece = do
    dataLength <- getWord16le   -- total length including the 2 bytes we just read, but excluding 4 byte footer
    getByteString $ fromIntegral dataLength - 2 + 4

bsPiecesHelper :: BS.ByteString -> [Either String BS.ByteString]
bsPiecesHelper bs = if BS.null bs
                    then []
                    else let (eith, remainingBS) = runGet getBSPiece bs
                         in eith:bsPiecesHelper remainingBS

bsPieces :: BS.ByteString -> Either String [Either String BS.ByteString]
bsPieces bs = let (eitherL, remBS) = runGet getWord32le bs
                  listOfBS = bsPiecesHelper remBS
              in case eitherL of
                  Left msg -> Left $ "Couldn't read number of items: " ++ msg
                  Right x  -> if length listOfBS /= fromIntegral x
                              then Left $ "Number of items from file " ++ show x ++
                                          " doesn't match number parsed " ++ show (length listOfBS)
                              else Right listOfBS

getTorchText :: Get T.Text
getTorchText = fromIntegral . (*2) <$> getWord16le >>= getByteString >>= \x -> return (decodeUtf16LE x)

getItem :: Get Item
getItem = do
    model <- getByteString 11
    name <- getTorchText
    prefix <- getTorchText
    suffix <- getTorchText
    serial <- getByteString 24
    bytes1 <- getByteString 29
    nEnchants <- fromIntegral <$> getWord32le
    location <- fromIntegral <$> getWord16le
    bytes2 <- getByteString 9
    bytes3 <- getByteString 8
    bytes4 <- sequence $ replicate 4 $ getByteString 20
    level <- fromIntegral <$> getWord32le
    bytes5 <- getByteString 4
    nSockets <- fromIntegral <$> getWord32le
    nUsedSockets <- fromIntegral <$> getWord32le
    bytes6 <- getByteString 4
    maxDmg <- fromIntegral <$> getWord32le
    armor <- fromIntegral <$> getWord32le
    bytes7 <- getByteString 4
    bytes8 <- getByteString 12
    nElements <- fromIntegral <$> getWord16le
    elements <- sequence $ replicate (fromIntegral nElements) $ getByteString 12
    theRest <- remaining >>= getByteString
    let (modsData, footer) = BS.spanEnd (==0) theRest
    return (Item model name prefix suffix serial bytes1 nEnchants location bytes2 bytes3
            bytes4 level bytes5 nSockets nUsedSockets bytes6 maxDmg armor bytes7 bytes8
            nElements elements (bsToMods modsData) footer)

bsToItems :: BS.ByteString -> Either String ([String], [Item])
bsToItems bs =
    if BS.null bs
    then return ([],[])
    else bsPieces bs >>=
         return . partitionEithers >>=
         return . snd >>=
         return . map (\x -> fst $ runGet getItem x) >>=
         return . partitionEithers

modDelimiter = toStrict $ runPut $ putWord32le 0x03

modPieces :: BS.ByteString -> [BS.ByteString]
modPieces bs =
    if BS.null bs
    then []
    else let (bsPiece, remainingBS) = BS.breakSubstring modDelimiter bs
         in bsPiece:modPieces (BS.drop (BS.length modDelimiter) remainingBS)

getMod :: Get Mod
getMod = do
    modType <- getWord32le
    modName <- getTorchText
    restData <- remaining >>= getByteString
    return $ Mod modType modName restData

bsToMods :: BS.ByteString -> Either String [Either String Mod]
bsToMods bs = let (eitherNMods, remBS) = runGet getWord32le bs
                  pieces = modPieces remBS
                  eitherList = map (fst . runGet getMod) pieces
              in case eitherNMods of
                  Left msg -> Left $ "Couldn't read number of mods: " ++ msg
                  Right x  -> if length eitherList /= fromIntegral x
                              then Left $ "Number of mods from item " ++ show x ++
                                          " doesn't match number parsed " ++ show (length eitherList)
                              else Right eitherList

