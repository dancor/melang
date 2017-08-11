{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.Trans
import Control.Monad.Trans.Resource
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.Text as CT
import qualified Data.HashTable.IO as H
import Data.Maybe
import Data.Monoid
import Data.Strict.Tuple
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.IO as DTI
import qualified Data.Vector.Mutable as VecM
import qualified Data.Vector as Vec

import qualified MediaWiki as MW

{-
myConduit :: Conduit Article (ResourceT IO) o
myConduit =
    CC.filter (\(title :!: body) -> any ("recurrir" `BS.isInfixOf`) body)
    =$= CC.map (\(title :!: body) -> title)
    =$= CC.take 1000
    =$= awaitForever (liftIO . BSC.putStrLn)
-}

-- "āáǎàēéěèīíǐìōóǒòūúǔù"

{-
convPron (c:rest) = 
  where
-}  

data DictEntry = DictEntry
  { eWord          :: DT.Text
  , ePron          :: DT.Text
  , eFreqs         :: DT.Text
  , ePartsOfSpeech :: DT.Text
  , eDef           :: DT.Text
  } deriving Show

type DictEntrys = VecM.IOVector DictEntry

readDictEntry :: DT.Text -> DictEntry
readDictEntry line = DictEntry word pron freqs partsOfSpeech def
  where
    [word, pron, freqs, partsOfSpeech, def] = DT.split (== '\t') line

showDictEntry :: DictEntry -> DT.Text
showDictEntry e =
    DT.intercalate "\t" [eWord e, ePron e, eFreqs e, ePartsOfSpeech e, eDef e]
    <> "\n"

type WordNum = H.BasicHashTable DT.Text Int

type Dict = (DictEntrys, WordNum)

readDict :: FilePath -> IO Dict
readDict filePath = do
    let numEntrys = 55739
    dictEntrys <- VecM.new numEntrys
    wordNum <- H.newSized numEntrys
    let myAccum entry num = liftIO $ do
            VecM.write dictEntrys num entry
            H.insert (wordNum :: WordNum) (eWord entry) num
            return (num + 1, [])
    runResourceT
        $  CC.sourceFile filePath
        $$ CC.linesUnbounded
        =$ CC.map readDictEntry
        =$ CC.concatMapAccumM myAccum 0
    {-
    res <- VecM.read dictEntrys 1234
    print $ res
    DTI.putStrLn $ eWord res
    res <- H.lookup wordNum "显然"
    print $ res
    -}
    return (dictEntrys, wordNum)

writeDict :: FilePath -> Dict -> IO ()
writeDict filePath (dictEntrys, _) = do
    v <- Vec.freeze dictEntrys
    Vec.mapM_ (DTI.appendFile filePath . showDictEntry) v

optimizePron = id

extractDef :: [BS.ByteString]
    -> Pair (Maybe String) (Maybe BS.ByteString)
extractDef body = pron :!: defs
  where
    chinSec = dropWhile (/= "==Chinese==") body
    pron1 = fmap (BS.drop 3) $ listToMaybe $
        dropWhile (not . BS.isPrefixOf "|m=") chinSec
    pron = optimizePron $
        fmap (takeWhile (/= ',') . DT.unpack . DTE.decodeUtf8) pron1
    defSubSec = takeWhile (not . BS.isPrefixOf "==") $ drop 1 $
        dropWhile (/= "===Definitions===") chinSec
    defs =
        -- (\x -> if null x then Nothing else Just $ BS.intercalate x) $
        listToMaybe $
        filter (/= "{{rfdef|zh}}") $
        filter (/= "{{rfdef|lang=zh}}") $
        map (BSC.filter (`notElem` ("[]" :: String)) . BS.drop 2) $
        filter (BS.isPrefixOf "# ") defSubSec

dictUpdate :: DT.Text -> Maybe DT.Text -> Maybe DT.Text -> Dict -> IO ()
dictUpdate word pronMb defMb (dictEntrys, wordNum) = do
    let modPron entry = case pronMb of
          Just pron ->
            if ePron entry == "?" then entry {ePron = pron} else entry
          _ -> entry
        modDef entry = case defMb of
          Just def ->
            if eDef entry == "?" then entry {eDef = def} else entry
          _ -> entry
    numMb <- H.lookup wordNum word
    case numMb of
      Just num -> VecM.modify dictEntrys (modPron . modDef) num
      _ -> return ()

myConduit :: Dict -> Conduit MW.Article (ResourceT IO) o
myConduit dict =
    CC.map (\(word :!: body) -> word :!: extractDef body)
    =$= CC.filter
        (\(_ :!: (pronMb :!: defMb)) -> isJust pronMb || isJust defMb)
    =$= awaitForever
        (\(word :!: (pronMb :!: defMb)) -> liftIO $ dictUpdate
            (DTE.decodeUtf8 word)
            (fmap DT.pack pronMb) (fmap DTE.decodeUtf8 defMb) dict
            )

main :: IO ()
main = do
    dict <- readDict "/home/danl/p/l/melang/lang/zh/dict.pre-add"
    runResourceT
        $  MW.bzxmlArticles "/home/danl/data/wikt/en.xml.bz2"
           (myConduit dict)
    writeDict "/home/danl/p/l/melang/lang/zh/dict" dict
