-- {-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI
import qualified Text.Parsec as P
import qualified Text.Parsec.Combinator as P
import qualified Text.Parsec.Prim as P
import qualified Text.Parsec.Pos as P

type T = DT.Text

wiktF :: String
wiktF = "/home/danl/data/wikt/es/articles.txt"

myMarkup :: String
myMarkup = "{{es.v.conj|irregular=sí|conjugacion=tercera|{{l*|es|ir}}|{{l*|es|yendo}}|{{l*|es|ido}}|{{l*|es|ido}}|{{l*|es|voy}}|{{l*|es|vas}}|{{l*|es|vas}}|{{l*|es|va}}|{{l*|es|vamos}}|{{l*|es|vais}}|{{l*|es|van}}|{{l*|es|iba}}|{{l*|es|ibas}}|{{l*|es|iba}}|{{l*|es|íbamos}}|{{l*|es|ibais}}|{{l*|es|iban}}|{{l*|es|fui}}|{{l*|es|fuiste}}|{{l*|es|fue}}|{{l*|es|fuimos}}|{{l*|es|fuisteis}}|{{l*|es|fueron}}|{{l*|es|iré}}|{{l*|es|irás}}|{{l*|es|irá}}|{{l*|es|iremos}}|{{l*|es|iréis}}|{{l*|es|irán}}|{{l*|es|iría}}|{{l*|es|irías}}|{{l*|es|iría}}|{{l*|es|iríamos}}|{{l*|es|iríais}}|{{l*|es|irían}}|{{l*|es|vaya}}|{{l*|es|vayas}}|{{l*|es|vayás}}|{{l*|es|vaya}}|{{l*|es|vayamos}}|{{l*|es|vayáis}}|{{l*|es|vayan}}|{{l*|es|fuera}}|{{l*|es|fueras}}|{{l*|es|fuera}}|{{l*|es|fuéramos}}|{{l*|es|fuerais}}|{{l*|es|fueran}}|{{l*|es|fuese}}|{{l*|es|fueses}}|{{l*|es|fuese}}|{{l*|es|fuésemos}}|{{l*|es|fueseis}}|{{l*|es|fuesen}}|{{l*|es|fuere}}|{{l*|es|fueres}}|{{l*|es|fuere}}|{{l*|es|fuéremos}}|{{l*|es|fuereis}}|{{l*|es|fueren}}|{{l*|es|ve}}|{{l*|es|andá}}*|{{l*|es|id}}|notas=* El imperativo &quot;andá&quot; es del verbo [[andar]], pero se usa comúnmente para el imperativo de ''ir'' en algunas zonas de [[voseo]].}}"

{-
procMarkup :: String -> [T]
procMarkup "" _ = ""
procMarkup ('{':'{':'{':rest) wikt = ""
procMarkup (c:rest) wikt = c : procMarkup rest wikt
--procMarkup s = error $ "procMarkup: " ++ take 100 (show s)

myRun = callTemplate "es.v.conj"

parse

procTemplateArgs :: T -> [T]

callTemplate :: T -> T -> [T] -> [T]
callTemplate templateName templateArgs wikt =
    
  where
    template = pullArticle ("Plantilla:" <> templateName) wikt
    args = 

pullArticle :: T -> [T] -> [T]
pullArticle articleName =
    takeWhile (not . ("^" `DT.isPrefixOf`)) .
    tail .
    dropWhile (/= DT.cons '^' articleName) 
-}

-- Wikimedia parsing of nowiki and curlies (curly braces):
--
-- - "<nowiki", any whitespace, ">" starts nowiki iff later there is
--   "</nowiki", any whitespace, ">"
-- - nowiki parsing happens first, then curly parsing
--
-- - Nested curlies have no grammatical meaning and outer extra curlies stay
--   literal: {{{{a}}}} -> { {{{a}}} }
-- - Further still, after {{{ if }} is found then the {{{ -> { {{
-- - After {{ or {{{ if } is found before }} or }}} then everything to that
--   point is quoted literally (as if a nowiki)

{-
wiktTwoBraceP :: Bool -> P.Parsec String u String
wiktTwoBraceP awaitCloseBraceAfter = liftA2 (++)
    ( P.char '{' *> (('3':) <$> wiktBraceP True) <* P.string "}}}"
      P.<|> 
      ('2':) <$> wiktBraceP True <* P.string "}}"
    )
    (wiktBraceP awaitCloseBraceAfter)

wiktOneBraceP :: Bool -> P.Parsec String u String
wiktOneBraceP awaitCloseBrace =
    P.char '{' *> wiktTwoBraceP awaitCloseBrace
    P.<|>
    ('{':) <$> wiktP

wiktBraceP :: Bool -> P.Parsec String u String
wiktBraceP awaitCloseBrace =
    P.char '{' *> wiktOneBraceP awaitCloseBrace
    P.<|>
    liftA2 (:)
        (if awaitCloseBrace then P.noneOf "}" else P.anyChar)
        (wiktBraceP awaitCloseBrace)
    P.<|>
    pure ""
-}

-- Level-1 parser: recognize nowiki tags.

data L1Item
  = L1String       {l1String :: String}
  | L1NowikiTag    {l1String :: String}
  | L1EndNowikiTag {l1String :: String}
  deriving Show

isL1NowikiTag (L1NowikiTag _) = True
isL1NowikiTag _ = False

isL1EndNowikiTag (L1EndNowikiTag _) = True
isL1EndNowikiTag _ = False

l1P :: P.Parsec [Char] st [L1Item]
l1P = P.many (P.try nowikiP P.<|> P.try endNowikiP P.<|> anyP)
  where
    nowikiP = L1NowikiTag <$>
        liftA2 (++) (P.string "<nowiki")
        (liftA2 (++) (P.many P.space) (P.string ">"))
    endNowikiP = L1EndNowikiTag <$>
        liftA2 (++) (P.string "</nowiki")
        (liftA2 (++) (P.many P.space) (P.string ">"))
    anyP = L1String . (:[]) <$> P.anyChar

-- Level-2 parser: recognize nowiki blocks.

data L2Item = L2String String | L2NowikiBlock String String String
  deriving Show

sat :: (Monad m, Show a) => (a -> Bool) -> P.ParsecT [a] u m a
sat f = P.tokenPrim show posNoUpdate (\c -> if f c then Just c else Nothing)

posNoUpdate :: P.SourcePos -> a -> [a] -> P.SourcePos
posNoUpdate pos _ _ = pos

l2P :: P.Parsec [L1Item] st [L2Item]
l2P = P.many (P.try nowikiP P.<|> anyP)
  where
    nowikiP = liftA3 L2NowikiBlock
        (          l1String <$> sat isL1NowikiTag)
        (concatMap l1String <$> P.many (sat (not . isL1EndNowikiTag)))
        (          l1String <$> sat isL1EndNowikiTag)
    anyP = L2String . l1String <$> P.anyToken

-- Level-3 parser: recognize curly-brace runs

{-
l3P :: P.Parsec [L2Item] st [L3Item]
l3P = P.many (P.try P.<|> anyP)
  where
    
-}
{-
data BlockItem = 

data BlockType = 

tagP :: P.Parsec [LexItem] st [?]
tagP = P.try (P.satisfy isNoWiki ) P.<|> 
-}

-- parseWikt :: 
parseWikt s = l2CollapseStr l2
  where
    Right l2 = P.parse l2P "lol2" l1
    Right l1 = P.parse l1P "lol1" s
    l2CollapseStr (L2String x : L2String y : rest) =
        l2CollapseStr (L2String (x ++ y) : rest)
    l2CollapseStr (x : xs) = x : l2CollapseStr xs
    l2CollapseStr [] = []

main :: IO ()
main = do
    print $ parseWikt "abc"
    print $ parseWikt "<nowiki>a</nowiki>"
    print $ parseWikt "a<c"
    print $ parseWikt "<<nowiki>"
    print $ parseWikt "</nowiki >"
    print $ parseWikt "<nowiki"
    {-
    print $ P.parse wiktP "lol" "a"
    print $ P.parse wiktP "lol" "{a}"
    print $ P.parse wiktP "lol" "{{a}}"
    print $ P.parse wiktP "lol" "{{{a}}}"
    print $ P.parse wiktP "lol" "{{a}}}}"
    print $ P.parse wiktP "lol" myMarkup
    -}
    --wikt <- DT.lines <$> DTI.readFile wiktF
    --mapM_ DTI.putStrLn (myRun wikt)
