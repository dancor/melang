module SigFig where

import Control.Arrow
import Text.Printf

-- XXX: todo: negative
sigFig :: Int -> Float -> String
sigFig numSigDigs x =
    if myMantissa == "0" then "0" else if myExponent >= 0
      then
        myMantissa ++ replicate myExponent '0'
      else
        if myExponent > - length myMantissa
          then
            (\ (a, b) -> a ++ "." ++ b) $
                splitAt (length myMantissa + myExponent) myMantissa
          else
            "0." ++ replicate (- length myMantissa - myExponent) '0' ++
                myMantissa
  where
    (xInt, xDec) = properFraction x
    xIntStr = show xInt
    xDecStr = drop 2 $ printf "%f" xDec
    (xDecLeadZeros, xDecRest) = span (== '0') xDecStr
    (myExponent, myMantissa) =
        first (subtract numSigDigs) .
        second (show . (`div` (10 :: Int)) . (+ 5) . read .
                take (numSigDigs + 1)) $
            if xInt == (0 :: Int)
              then (- length xDecLeadZeros, xDecRest ++ repeat '0')
              else (length xIntStr, xIntStr ++ xDecStr ++ repeat '0')
