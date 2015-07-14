#include <h>

main :: IO ()
main = do
    ls <- init . tail . DT.splitOn "\t" <$> DTI.readFile (
        "/home/danl/data/goog-ngrams/20120701/ger/1grams" </>
        "googlebooks-ger-all-totalcounts-20120701.txt")
    let goodYearLs =
            filter ((>= (1980 :: Int)) . read . DT.unpack . head) $
            map (DT.splitOn ",") ls
        goodYearCounts = map (read . DT.unpack . (!! 1)) goodYearLs
        theSum = sum goodYearCounts :: Integer
    print theSum
