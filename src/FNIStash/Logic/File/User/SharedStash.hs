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

module FNIStash.Logic.File.User.SharedStash (
    getScrambled,
    descrambleGameFile,
    scrambleGameFile,
    fileGameData,
    unScrambled,
    unDescrambled,
    bsToItems
) where

import FNIStash.Logic.File.User.Crypto
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
import qualified Data.Map as M
import Debug.Trace (trace)
import Control.Monad.Loops
import Control.Monad

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
    return (Item model name prefix suffix serial bytes1 nEnchants location bytes2 bytes3
            bytes4 level bytes5 nSockets nUsedSockets bytes6 maxDmg armor bytes7 bytes8
            nElements elements (bsToModLists theRest))

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
m804A = getByteString 39
m8149 = getByteString 45
m8041 = lookAhead getWord8 >>= \x -> getByteString (1 + 30 + 4* fromIntegral x + 4)
m8441 = m8041
m8141 = getByteString 45
m8050 = m8041
m8058 = getByteString 39
mA141 = getByteString 53
mA041 = getByteString 51
m8048 = getByteString 39
m8042 = getByteString 33
m9041 = getByteString 59
m8051 = getByteString 67
mDefault = getByteString 45


modTypeSizeMap = M.fromList [(0x804A, m804A), (0x8041, m8041), (0x8441, m8441), (0x8149, m8149),
                             (0x8050, m8050), (0x8141, m8141), (0x8058, m8058), (0xA141, mA141),
                             (0xA041, mA041), (0x8048, m8048), (0x8042, m8042), (0x9041, m9041),
                             (0x8051, m8051)]


lkModReader modType = M.findWithDefault mDefault modType modTypeSizeMap

getMod :: Get Mod
getMod = do
    modType <- getWord32le
    modName <- getTorchText
    restData <- lkModReader modType
    return $ Mod modType modName restData

getModList :: Get [Mod]
getModList = do
    modCount <- getWord32le
    modList <- replicateM (fromIntegral modCount) getMod
    when (fromIntegral modCount /= length modList)
        (fail $ "Number of mods in list " ++ show modCount ++ " doesn't match number parsed " ++ (show $ length modList))
    whileM (andM [moreBS, isZeroNext getWord32le])
        getWord32le -- discard 0's after reading last mod
    return modList

moreBS = remaining >>= \x -> return (0/=x)
isZeroNext m = lookAhead m >>= \x -> return (0==x)

getModLists = whileM moreBS getModList

bsToModLists :: BS.ByteString -> Either String [[Mod]]
bsToModLists bs = fst $ runGet getModLists bs



