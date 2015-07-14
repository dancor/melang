#include <h>

titlePrefix :: DT.Text
titlePrefix = "    <title>"

titlePrefixLen :: Int
titlePrefixLen = DT.length titlePrefix

altPrefix :: DT.Text
altPrefix = "{{Alte Schreibweise|"

altPrefixLen :: Int
altPrefixLen = DT.length altPrefix

findAlts :: DT.Text -> DT.Text -> (DT.Text, Maybe (DT.Text, DT.Text))
findAlts l prevTitle = if titlePrefix `DT.isPrefixOf` l
  then (DT.takeWhile (/= '<') $ DT.drop titlePrefixLen l, Nothing)
  else if altPrefix `DT.isPrefixOf` l
    then
      ( ""
      , Just
        ( prevTitle
        , DT.takeWhile (\c -> c /= '|' && c /= '}' && c /= '#') $
          DT.drop altPrefixLen l
        )
      )
    else (prevTitle, Nothing)

main :: IO ()
main = runResourceT $ CC.sourceFile "/home/danl/data/wikt/de/cur"
    $$ CC.linesUnbounded
    =$ void (CL.mapAccum findAlts "")
    =$ CL.catMaybes
    =$ CC.map (\(wd, repl) -> wd <> "\t" <> repl)
    =$ CC.unlines
    =$ CC.sinkFile "out"
