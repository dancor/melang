{-# LANGUAGE DeriveGeneric #-}

module Lang.Zh.WdPinyinGloss where

import Codec.Serialise
import qualified Data.Aeson as Ae
import Data.Text (Text)
import qualified Data.Vector as VL
import GHC.Generics

data WdPinyin = WdPinyin !Text !Text !Text deriving (Generic)

instance Serialise WdPinyin

data WdPinyinGloss = WdPinyinGloss !Text !Text !Text deriving (Generic)

instance Serialise WdPinyinGloss

wdPinyinGlossToAeson :: WdPinyinGloss -> Ae.Value
wdPinyinGlossToAeson (WdPinyinGloss wd pinyin gloss) =
    Ae.Array $ VL.fromList $ map Ae.String [wd, pinyin, gloss]

wdPinyinGlossesToAeson :: [WdPinyinGloss] -> Ae.Value
wdPinyinGlossesToAeson = Ae.Array . VL.fromList . map wdPinyinGlossToAeson

