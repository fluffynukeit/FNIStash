-----------------------------------------------------------------------------
--
-- Module      :  Main
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

testDir = "C:\\Users\\Dan\\Desktop\\FNI Testing"
ssFileOrig = testDir </> "sharedstash_v2.bin"
ssDescrambled = testDir </> "sharedstash_haskell.bin"
ssScrambled = testDir </>  "sharedstash_haskellScrambled.bin"
examineFile = testDir </>  "shareStashExamine.txt"
pakManFileBinary = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK.MAN"
pakFileBinary = "C:\\Program Files (x86)\\Steam\\steamapps\\common\\Torchlight II\\PAKS\\DATA.PAK"
pakManFileText = testDir </>  "pakMan.txt"
pakTestFile = "Media\\Shadows.png"
--pakTestFile = "Media\\Affixes.raw"

main = do
    input <- BS.readFile ssFileOrig
    writeFile examineFile $ inputToString input
    let (Right a, bs) = runGet (getScrambled >>= return . fileGameData . unDescrambled . descrambleGameFile) input
    BS.writeFile ssDescrambled a
    (Right man, bs) <- readPAKMAN pakManFileBinary
    writeFile pakManFileText $ (show man) ++ "\n\n All files: \n" ++ (unlines $ pakFileList man)
    pakFiles <- readPAKFiles pakManFileBinary pakFileBinary
    let posixName = (P.joinPath $ splitDirectories pakTestFile)
        maybeTestFileData = lkupPAKFile pakFiles posixName
    BSL.writeFile (testDir </> pakTestFile) $ case maybeTestFileData of
        Just testFileData -> testFileData
        _ -> BSLC.pack $ "Media file " ++ pakTestFile ++ " with Posix name " ++ posixName ++
                         " does not exist in PAKFile mapping:\n" ++ (unlines $ keys pakFiles)
    getLine


    

inputToString :: BS.ByteString -> String
inputToString input =
    let k = (fst $ runGet getScrambled input) >>=
            return . descrambleGameFile >>=
            bsToItems . fileGameData . unDescrambled
    in either id (\(failItems, succItems) ->
                    unlines ["N Items successfully parsed: " ++ (show $ length succItems),
                            "",
                            concatMap show succItems]) k


