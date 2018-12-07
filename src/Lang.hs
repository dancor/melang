module Lang where

data Lang = Cmn | Spa deriving (Show, Eq)

strToLang :: String -> Lang
strToLang "cmn" = Cmn
strToLang "spa" = Spa
strToLang s = error $ "strToLang: unknown language string: " ++ s

langToStr Cmn = "cmn"
langToStr Spa = "spa"
