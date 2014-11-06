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

import Data.List.Split
import System.Environment

-- | Type for PSSP data from http://pssp.srv.ualberta.ca/.
data InputLine = InputLine { time :: Double  -- ^ Time of event, or censoring.
                           , censored :: Bool  -- ^ True if censored, False otherwise.
                           , features :: [(Integer, Double)]  -- ^ (Feature, Value).
                           }

instance Show InputLine where
  show inp = t ++ " " ++ " " ++ c ++ f
       where t = show $ time inp
             c = if censored inp then "1" else "0"
             f = show $ features inp

-- | Read a line from the http://pssp.srv.ualberta.ca/ dataset.
lineRead :: String -> InputLine
lineRead str = InputLine (read t) (if read c == 0 then False else True) features
         where (t:c:featureStrs) = words str
               features = map readFeature featureStrs
               readFeature f = (read number, read value)
                 where [number, value] = splitOn ":" f

main :: IO ()
main = do [file] <- getArgs
          fileData <- readFile file
          (print . map lineRead . lines) fileData
