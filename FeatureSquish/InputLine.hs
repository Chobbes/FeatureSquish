{- Copyright (C) 2014 Calvin Beck

   Permission is hereby granted, free of charge, to any person
   obtaining a copy of this software and associated documentation files
   (the "Software"), to deal in the Software without restriction,
   including without limitation the rights to use, copy, modify, merge,
   publish, distribute, sublicense, and/or sell copies of the Software,
   and to permit persons to whom the Software is furnished to do so,
   subject to the following conditions:

   The above copyright notice and this permission notice shall be
   included in all copies or substantial portions of the Software.

   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
   EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
   MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
   NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
   BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
   ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
   CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
   SOFTWARE.
-}


module FeatureSquish.InputLine (InputLine (..), linesToCSV) where

import Data.List


-- | Type for PSSP data from http://pssp.srv.ualberta.ca/.
data InputLine = InputLine { time :: Double  -- ^ Time of event, or censoring.
                           , censored :: Bool  -- ^ True if censored, False otherwise.
                           , features :: [(Integer, Double)]  -- ^ (Feature, Value).
                           }

instance Show InputLine where
  show inp = t ++ " " ++ c ++ " " ++ f
       where t = show $ time inp
             c = if censored inp then "1" else "0"
             f = unwords . map (\(f,v) -> show f ++ ":" ++ show v) $ features inp


-- | Convert an InputLine to a CSV representation.
linesToCSV :: [InputLine] -> String
linesToCSV inps = intercalate "\n" (header : inputsCSV)
  where header = "Time, Censored, " ++ intercalate ", " (map show featureList)
        inputsCSV = map (inputLineToCSV featureList) inps
        featureList = nub $ concatMap (map fst . features) inps

-- | Convert an InputLine to CSV given a list of Integer features.
inputLineToCSV :: [Integer] -> InputLine -> String
inputLineToCSV featureList inp = intercalate ", " (t : c : featureStrings)
  where t = show $ time inp
        c = if censored inp then "1" else "0"
        featureStrings = map (maybeShow . flip lookup (features inp)) featureList
        maybeShow = maybe " " show
