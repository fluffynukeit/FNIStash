-----------------------------------------------------------------------------
--
-- Module      :  Main
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


module Main (
    main
) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import FNIStash.File.SharedStash
import Data.Binary.Strict.Get (runGet)
import FNIStash.File.PAK
import System.FilePath
import qualified System.FilePath.Posix as P
import Data.Map
import qualified Data.Text as T
import qualified Data.Text.IO as T
import FNIStash.File.DAT
import FNIStash.Logic.Variables

testDir = "C:\\Users\\Dan\\Desktop\\FNI Testing"
ssFileOrig = testDir </> "sharedstash_v2.bin"
ssDescrambled = testDir </> "sharedstash_haskell.bin"
ssScrambled = testDir </>  "sharedstash_haskellScrambled.bin"
examineFile = testDir </>  "shareStashExamine.txt"
pakManFileBinary = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK.MAN"
pakFileBinary = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK"
pakManFileText = testDir </>  "pakMan.txt"
--pakTestFile = "Media\\Shadows.png"
pakTestFile = "Media\\Affixes.raw"
effectListFile = "Media\\EffectsList.dat"

main = do
    input <- BS.readFile ssFileOrig
    writeFile examineFile $ inputToString input
    let (Right a, bs) = runGet (getScrambled >>= return . fileGameData . unDescrambled . descrambleGameFile) input
    BS.writeFile ssDescrambled a
    man <- readPAKMAN pakManFileBinary
    let pakFiles = readPAKFiles man pakFileBinary
        posixName = (P.joinPath $ splitDirectories pakTestFile)
        maybeIOGetFile = lkupPAKFile pakFiles posixName
    BSL.writeFile (testDir </> pakTestFile) =<< case maybeIOGetFile of
        Just testFileData -> testFileData
        _ -> return . BSLC.pack $ "Media file " ++ pakTestFile ++ " with Posix name " ++ posixName ++
                         " does not exist in PAKFile mapping:\n" ++ (T.unpack . T.unlines $ keys pakFiles)
    let effectFilePosix = P.joinPath $ splitDirectories effectListFile
        Just effectListDataAction = lkupPAKFile pakFiles effectFilePosix
    effectBS <- effectListDataAction
    let (Right dat, extra) = runGet getDAT (BSLC.toStrict effectBS)
    T.writeFile (testDir </> effectListFile) (textDAT dat)
    T.writeFile (testDir </> "testOutput.txt") $
        (let k = return dat >>= sectionAt 130 >>= findVar vEFFECT >>= textVar
         in case k of
            Just a -> a
            Nothing -> T.pack "Couldn't find variable!!"
            )


    

inputToString :: BS.ByteString -> String
inputToString input =
    let k = (fst $ runGet getScrambled input) >>=
            return . descrambleGameFile >>=
            bsToItems . fileGameData . unDescrambled
    in either id (\(failItems, succItems) ->
                    unlines ["N Items successfully parsed: " ++ (show $ length succItems),
                            "",
                            concatMap show succItems]) k


