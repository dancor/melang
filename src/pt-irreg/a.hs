#include <h>

-- Only imperative irreg I know is for dizer, the irreg "tu diz!" is okay,
-- as well as the regular "tu dize!" Same for fazer, querer, trazer.
-- Also for ser it's the irreg "tu sê!" Note Pres2s ending "ns" -> "m"

-- Tenses that are always regular: Plup SFut

-- - The only verbs where SPres doesn't match Pres1s are 7:
--   dar estar haver ir querer saber ser
--   Only dar and ir have finer irregularities.
-- - For Fut and Cond, only 3 irregs: dizer->dir fazer->far trazer->trar
-- - SImperf from Pret3p except ir&ser and pôs. SFut always!
-- - For SImperf: Vsse Vsses Vsse  Wssemos Wsseis Vssem
--   where V->W is: a->á e->ê(but-é-for-pôr) i->í o->ô
-- - The Plup is always formed from Pret3p, with same accent changes.
-- - Imperf (same accents) 4: pôr:punha, ser:era, ter:tinha, vir:vinha
-- - I believe all Gerunds are regular.
-- - There are some irregular Participles. Only for vir does it equal the gerund

data Tense = Pres | SPres | Imperf | Pret | Plup | SImperf | SFut | Fut |
  Cond | Participle deriving (Eq, Ord, Show)

data Person = S1 | S2 | S3 | P1 | P2 | P3
  deriving (Eq, Ord, Show)

data VerbInfo = VerbInfo
  { vInfinitive :: Text
  , vExceptions :: Map Tense (Map Person Text)
  } deriving Show

tLastNote n x = if T.null x then error $ "lastNote: " ++ n else T.last x

conj, cnj :: VerbInfo -> Tense -> Person -> Text
conj v t p =  case Map.lookup t $ vExceptions v of
  Just m -> case Map.lookup p m of Just r -> r; _ -> cnj v t p
  _ -> cnj v t p

-- Pres: S3->S12P13 P1->P2
-- Pret: P1->S2P23

cnj v Pres S1 = let x = conj v Pres S3 in case T.takeEnd 1 x of
  "r" -> x <> "o"
  "i" -> x <> "o"
  "m" -> T.init x <> "nho"
  _   -> T.init x <> "o"
cnj v Pres S2 = let x = conj v Pres S3 in case tLastNote "1" x of
  'm' -> T.init x <> "ns"; 'r' -> x <> "es"; 'z' -> x <> "es"; _ -> x <> "s"
cnj v Pres S3 = vStem v <> if x == "i" then "e" else x where x = vVow v
cnj v Pres P1 = let x = presS3ConsE v in (<> "mos") $ case tLastNote "2" x of
  'á' -> T.init x <> "a"
  'ê' -> T.init x <> "e"
  'm' -> T.init x
  _ -> if vVow v == "i" then T.init x <> "i" else x
-- Note this one also applies to some other tenses besides Pres:
cnj v Pres P2 = x <> if T.takeEnd 1 x == "i" then "s" else "is"
  where x = T.dropEnd 3 (conj v Pres P1)
cnj v Pres P3 = let x = presS3ConsE v in
    if T.takeEnd 1 x == "ê" then T.init x <> "eem" else x <> "m"

cnj v SPres S1 = T.init (conj v Pres S1) <> if vVow v == "a" then "e" else "a"
cnj v SPres S2 = conj v SPres S1 <> "s"
cnj v SPres S3 = conj v SPres S1
cnj v SPres P1 = conj v SPres S1 <> "mos"
cnj v SPres P2 = conj v SPres S1 <> "is"
cnj v SPres P3 = conj v SPres S1 <> "m"

cnj v Pret S1 = vStem v <> if vVow v == "a" then "ei" else "i"
cnj v Pret S2 = pretP1Mod v <> "ste"
cnj v Pret S3 = vStem v <> (case vVow v of "a" -> "o"; x -> x) <> "u"
cnj v Pret P1 = vStem v <> (if x == "a" then "á" else x) <> "mos"
  where x = vVow v
cnj v Pret P2 = pretP1Mod v <> "stes"
cnj v Pret P3 = pretP1Mod v <> "ram"

cnj v Imperf S1 = imperfP1Mod2 v
cnj v Imperf S2 = imperfP1Mod2 v <> "s"
cnj v Imperf S3 = imperfP1Mod2 v
cnj v Imperf P1 = vStem v <> if vVow v == "a" then "ávamos" else "íamos"
cnj v Imperf P2 = T.init (imperfP1Mod v) <> "eis"
cnj v Imperf P3 = imperfP1Mod2 v <> "m"

cnj v Plup S1 = T.dropEnd 3 (conj v Pret P3) <> "ra"
cnj v Plup S2 = conj v Plup S1 <> "s"
cnj v Plup S3 = conj v Plup S1
cnj v Plup P1 = y <> z <> "ramos" where
    x = T.dropEnd 2 (conj v Plup S1)
    y = T.dropEnd 1 x
    z = case T.takeEnd 1 x of
      "a" -> "á"; "e" -> "é"; "i" -> "í"; "o" -> "ô"; z2 -> z2
cnj v Plup P2 = T.dropEnd 4 (conj v Plup P1) <> "eis"
cnj v Plup P3 = conj v Plup S1 <> "m"

cnj v SImperf S1 = T.dropEnd 3 (conj v Pret P3) <> "sse"
cnj v SImperf S2 = conj v SImperf S1 <> "s"
cnj v SImperf S3 = conj v SImperf S1
cnj v SImperf P1 = y <> z <> "ssemos" where
    x = T.dropEnd 3 (conj v SImperf S1)
    y = T.dropEnd 1 x
    z = case T.takeEnd 1 x of
      "a" -> "á"; "e" -> if y == vStem v then "ê" else "é"; "i" -> "í";
      "o" -> "ô"; z2 -> z2
cnj v SImperf P2 = T.dropEnd 3 (conj v SImperf P1) <> "is"
cnj v SImperf P3 = conj v SImperf S1 <> "m"

cnj v Fut S1 = T.init (conj v Fut S3) <> "ei"
cnj v Fut S2 = conj v Fut S3 <> "s"
cnj v Fut S3 = vInfinitive v <> "á"
cnj v Fut P1 = T.init (conj v Fut S3) <> "emos"
cnj v Fut P2 = T.init (conj v Fut S3) <> "eis"
cnj v Fut P3 = T.init (conj v Fut S3) <> "ão"
cnj v Cond S1 = T.init (conj v Fut S3) <> "ia"
cnj v Cond S2 = T.init (conj v Fut S3) <> "ias"
cnj v Cond S3 = T.init (conj v Fut S3) <> "ia"
cnj v Cond P1 = T.init (conj v Fut S3) <> "íamos"
cnj v Cond P2 = T.init (conj v Fut S3) <> "íeis"
cnj v Cond P3 = T.init (conj v Fut S3) <> "iam"

cnj v SFut S1 = T.dropEnd 2 (conj v Pret P3)
cnj v SFut S2 = conj v SFut S1 <> "es"
cnj v SFut S3 = conj v SFut S1
cnj v SFut P1 = conj v SFut S1 <> "mos"
cnj v SFut P2 = conj v SFut S1 <> "des"
cnj v SFut P3 = conj v SFut S1 <> "em"

cnj v Participle _ = vStem v <> if vVow v == "a" then "ado" else "ido"

pretP1Mod v = if T.takeEnd 1 x == "á" then T.init x <> "a" else x
  where x = T.dropEnd 3 (conj v Pret P1)
    
presS3ConsE v = x <> if T.takeEnd 1 x `elem` ["r","z"] then "e" else ""
  where x = conj v Pres S3

imperfP1Mod v = T.dropEnd 3 (conj v Imperf P1)
imperfP1Mod2 = T.map f . imperfP1Mod where
    f 'á' = 'a'; f 'é' = 'e'; f 'í' = 'i'; f 'ú' = 'u'; f x = x

mkV :: Text -> [(Tense, [(Person, Text)])] -> VerbInfo
mkV i = VerbInfo i . Map.fromList . map (second Map.fromList)

vStem :: VerbInfo -> Text
vStem v = let i = vInfinitive v in case i of
  "pôr" -> "pô"
  _ -> T.dropEnd 2 i

vVow :: VerbInfo -> Text
vVow v = let i = vInfinitive v in case i of
  "pôr" -> "e"
  _ -> T.take 1 $ T.takeEnd 2 i

s1 = (,) S1
s2 = (,) S2
s3 = (,) S3
p1 = (,) P1
p2 = (,) P2
p3 = (,) P3
pres   = (,) Pres
sPres  = (,) SPres
pret   = (,) Pret
imperf = (,) Imperf
fut    = (,) Fut
partic x = (Participle, [(S1, x)])

-- fazer change z->ç could be detected?
vInfos :: [VerbInfo]
vInfos = [
    mkV "falar"   []
  , mkV "comer"   []
  , mkV "partir"  []
  , mkV "crer"    [pres  [s1"creio" ,s3"crê"   ,p2"credes"  ]]
  , mkV "dar"     [pres  [s1"dou"   ,s3"dá"    ,p3"dão"     ]
                  ,sPres [s1"dê"               ,p2"deis"    ]
                  ,pret  [s3"deu"   ,p1"demos"              ]]
  , mkV "dizer"   [pres  [s1"digo"  ,s3"diz"                ]
                  ,pret  [s1"disse" ,s3"disse" ,p1"dissemos"]
                  ,fut   [s3"dirá"]
                  ,partic"dito"]
  , mkV "estar"   [pres  [s1"estou" ,s3"está"  ,p3"estão"]
                  ,sPres [s1"esteja"]
                  ,pret  [s1"estive",s3"estive",p1"estivemos"]]
  , mkV "fazer"   [pres  [s1"faço"  ,s3"faz"]
                  ,pret  [s1"fiz"   ,s3"fez"   ,p1"fizemos"]
                  ,fut   [s3"fará"]
                  ,partic"feito"]
  , mkV "haver"   [pres  [s1"hei"   ,s3"há"    ,p1"havemos",p3"hão"]
                  ,sPres [s1"haja"]
                  ,pret  [s1"houve" ,s3"houve" ,p1"houvemos"]]
  , mkV "ir"      [pres  [s1"vou"   ,s3"vai"   ,p1"vamos",p2"ides" ,p3"vão"]
                  ,sPres [s1"vá"               ,p1"vamos",p2"vades",p3"vão"]
                  ,pret  [s1"fui"   ,s3"foi"   ,p1"fomos"]]
  , mkV "ler"     [pres  [s1"leio"  ,s3"lê"    ,p2"ledes"]]
  , mkV "medir"   [pres  [s1"meço"]]
  , mkV "ouvir"   [pres  [s1"ouço"]]
  , mkV "pedir"   [pres  [s1"peço"]]
  , mkV "perder"  [pres  [s1"perco"]]
  , mkV "poder"   [pres  [s1"posso"]
                  ,pret  [s1"pude" ,s3"pôde",p1"pudemos"]]
  , mkV "pôr"     [pres  [s1"ponho",s3"põe" ,p1"pomos",p2"pondes"]
                  ,pret  [s1"pus"  ,s3"põs" ,p1"pusemos"]
                  ,imperf[p1"púnhamos"]
                  ,partic"posto"]
  , mkV "querer"  [pres  [s3"quer"]
                  ,sPres [s1"queira"]
                  ,pret  [s1"quis",s3"quis",p1"quisemos"]]
  , mkV "rir"     [pres  [s3"ri"  ,p2"rides"]]
  , mkV "saber"   [pres  [s1"sei"]
                  ,sPres [s1"saiba"]
                  ,pret  [s1"soube",s3"soube",p1"soubemos"]]
  , mkV "ser"     [pres  [s1"sou"  ,s3"é",p1"somos",p3"são"]
                  ,sPres [s1"seja"]
                  ,pret  [s1"fui"  ,s3"foi"  ,p1"fomos"]
                  ,imperf[p1"éramos"]]
  , mkV "ter"     [pres  [s3"tem" ,p2"tendes",p3"têm"]
                  ,pret  [s1"tive",s3"teve",p1"tivemos"]
                  ,imperf[p1"tínhamos"]]
  , mkV "trazer"  [pres  [s1"trago" ,s3"traz"]
                  ,pret  [s1"trouxe",s3"trouxe",p1"trouxemos"]
                  ,fut   [s3"trará"]]
  , mkV "valer"   [pres  [s1"valho"]]
  , mkV "ver"     [pres  [s1"vejo" ,s3"vê",p2"vedes"]
                  ,pret  [s3"viu"  ,p1"vimos"]
                  ,partic"visto"]
  , mkV "vir"     [pres  [s3"vem" ,p1"vimos",p2"vindes",p3"vêm"]
                  ,pret  [s1"vim"  ,s3"veio",p1"viemos"]
                  ,imperf[p1"vínhamos"]
                  ,partic"vindo"]
  -- Radical-changing verbs:
  , mkV "boiar"    [pres [s3"bóia",p1"boiamos"]
                   ,sPres[s1"boie"]]
  , mkV "recear"   [pres [s3"receia",p1"receamos"]
                   ,sPres[p1"receemos",p2"receeis"]]
  , mkV "odiar"    [pres [s3"odeia",p1"odiamos"]
                   ,sPres[p1"odiemos",p2"odieis"]]
  , mkV "erguer"   [pres [s1"ergo"]]
  , mkV "conseguir"[pres [s1"consigo"]]
  , mkV "divertir" [pres [s1"divirto"]]
  , mkV "mentir"   [pres [s1"minto"]]
  , mkV "repetir"  [pres [s1"repito"]]
  , mkV "seguir"   [pres [s1"sigo"]]
  , mkV "sentir"   [pres [s1"sinto"]]
  , mkV "servir"   [pres [s1"sirvo"]]
  , mkV "vestir"   [pres [s1"visto"]]
  , mkV "cobrir"   [pres [s1"cubro"],partic"coberto"]
  , mkV "descobrir"[pres [s1"descubro"]]
  , mkV "dormir"   [pres [s1"durmo"]]
  , mkV "subir"    [pres [s1"subo",s3"sobe",p1"subimos"]]
  -- Irreg participles:
  , mkV "abrir"    [partic"aberto"]
  , mkV "escrever" [partic"escrito"]
  , mkV "ganhar"   [partic"ganho"]
  , mkV "gastar"   [partic"gasto"]
  , mkV "pagar"    [partic"pago"]
  -- 2nd irreg participles:
  , mkV "aceitar"  [partic"aceite"]
  , mkV "acender"  [partic"aceso"]
  , mkV "atender"  [partic"atento"]
  , mkV "entregar" [partic"entregue"]
  , mkV "enxugar"  [partic"enxuto"]
  , mkV "expulsar" [partic"expulso"]
  , mkV "limpar"   [partic"limpo"]
  , mkV "matar"    [partic"morto"]
  , mkV "nascer"   [partic"nato"]
  , mkV "omitir"   [partic"omisso"]
  , mkV "prender"  [partic"preso"]
  , mkV "romper"   [partic"roto"]
  , mkV "salvar"   [partic"salvo"]
  , mkV "secar"    [partic"seco"]
  , mkV "soltar"   [partic"solto"]
  , mkV "suspender"[partic"suspenso"]
  ]

main :: IO ()
main = mapM_ f vInfos where
    f v = T.putStrLn $ T.unlines $
        (vInfinitive v <> " " <> conj v Participle S1:)
        [T.intercalate " " $ map (conj v t) [S1,S2,S3,P1,P2,P3]
        -- | t <- [Pres,SPres,Imperf,Pret,Plup,SImperf,SFut,Fut,Cond]]
        | t <- [Pres,Imperf,Pret,Plup,Fut,Cond,SPres,SImperf,SFut]]
