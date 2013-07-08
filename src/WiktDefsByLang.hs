import Control.Arrow
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.HashMap.Strict as HMS
import System.Directory
import System.FilePath

import Util.BS

main :: IO ()
main = do
    homeDir <- getHomeDirectory
    let dataDir = homeDir </> "data"
        outDir = dataDir </> "wikt-by-lang"
    ls <- readLines $ dataDir </> "enwikt-defs-20130623-all.tsv"
    createDirectory outDir
    mapM_ (\(f, l) -> BS.writeFile (outDir </> BSC.unpack f) $ BSC.unlines l) .
        HMS.toList . HMS.fromListWith (++) $
        map (second (:[]) . breakTab) ls
