import Control.Applicative
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Set as Set
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.IO as DTI

import Cmn.Cedict
import Cmn.GoogBk

main :: IO ()
main = do
    cedictWdSet <- Set.fromList . map cSimp <$> loadCedict
    goog <- take 2000 <$> loadGoogBk
    putStrLn "Considering 2000 words."
    let googLeft = filter ((`Set.notMember` cedictWdSet) . gWd) goog
    wiktWdSet <- Set.fromList .
        map (DTE.decodeUtf8 . BSL.toStrict) . BSLC.lines <$>
        BSL.readFile "/home/danl/data/wikt/pages.txt"
    let googLeft2 = filter ((`Set.notMember` wiktWdSet) . gWd) googLeft
    putStrLn $ show (length googLeft2) ++ " words unverified."
    DTI.putStr $ DT.unlines $ take 80 $
        map (\g -> DT.concat [gWd g, gPos g]) googLeft2
