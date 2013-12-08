import Control.Applicative
import qualified Data.Text.IO as DTI

import Cmn.Dict
import Cmn.TextWds

main :: IO ()
main = do
    dict <- dictToWdDict <$> loadDict
    let maxWdLen = 7
    DTI.interact (textWdsHtml dict maxWdLen)
