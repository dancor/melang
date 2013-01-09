import Control.Applicative
import Data.Char
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Map as M

import NGrams

getWords :: BS.ByteString -> [BS.ByteString]
getWords =
    filter (not . BS.null) .
    -- map (BSC.map toLower . BSC.filter isAlpha) . 
    concatMap BSC.words . BSC.lines .
    BSC.map (\ c -> if isAlpha c then toLower c else ' ')

main :: IO ()
main = do
    ws <- getWords <$> BS.readFile "mobydick.txt"

    -- putStr . unlines . map show . reverse . sort . 
    --     map (\ x -> (length x, head x)) . group . sort $ ws
    
    wdToFreq <- M.fromListWith (+) . map (\ ng -> (ngWord ng, ngFreq ng)) <$>
        readNGrams "/home/danl/p/l/melang/data/ngrams/cur/eng/out/1980l"

    let wNub = nub $ sort ws
        wFreq = map (\ w -> (M.findWithDefault 0 w wdToFreq, w)) wNub
     
    putStr . unlines . map show . reverse $ sort wFreq
