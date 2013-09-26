{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import System.FilePath

import Cmn.Lcmc
import Cmn.KiloDeck

main :: IO ()
main = do
    wdLwiMap <- Map.fromList . map (\lwi -> (lwiWd lwi, lwi)) <$> loadLwis
    kDeck <- loadKiloDeck (kiloDir </> "mando-gloss-1k.txt")
    let procKLine kLine =
            case lwiMb of
              Nothing -> Nothing
              Just lwi ->
                if
                    takeWhile (/= ':') (snd . head $ lwiPos lwi) /=
                        DT.unpack
                        (DT.takeWhile (/= ':') . killNum $ kLGloss kLine)
                  then
                    Just $ DT.intercalate ": "
                    [ showKiloLine kLine
                    , maybe "XXX" showLwi $ Map.lookup (kLWord kLine) wdLwiMap
                    ]
                  else Nothing
          where
            lwiMb = Map.lookup (kLWord kLine) wdLwiMap
    mapM_ DTI.putStrLn . catMaybes $ map procKLine kDeck
