module Lang where

data Lang = Cmn | Spa deriving (Show, Eq)

strToLang :: String -> Lang
strToLang "cmn" = Cmn
strToLang "spa" = Spa

langToStr :: Lang -> String
langToStr Cmn = "cmn"
langToStr Spa = "spa"
