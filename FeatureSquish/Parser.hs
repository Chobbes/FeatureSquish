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

{-# LANGUAGE OverloadedStrings #-}

module FeatureSquish.Parser (parsePSSP) where

import FeatureSquish.InputLine

import Control.Applicative
import Control.Arrow
import Data.Attoparsec.Text
import Data.Maybe
import Prelude hiding (takeWhile)

-- | Parse PSSP data, either in CSV or MTLR format.
parsePSSP :: Parser [InputLine]
parsePSSP = parseCSV <|> parseMTLR

-- | Parse a file of MTLR data.
parseMTLR :: Parser [InputLine]
parseMTLR = do inps <- many parseMTLRLine
               endOfInput
               return inps

-- | Parse a single line of MTLR data.
parseMTLRLine :: Parser InputLine
parseMTLRLine = do time <- double
                   skipSpace
                   censored <- decimal
                   skipSpace
                   features <- many parseMTLRFeature
                   endOfLine'
                   return (InputLine time (censored /= 0) features)

-- | Parse a single feature:value pair for MTLR.
parseMTLRFeature :: Parser (Integer, Double)
parseMTLRFeature = do skipSpace
                      feature <- decimal
                      char ':'
                      value <- double
                      return (feature, value)

-- | Parse a CSV file of data.
parseCSV :: Parser [InputLine]
parseCSV = do header <- (do csv <- parseCSVLine; return [csv]) <|> (skipToEndOfLine *> return [])
              inps <- many parseCSVLine
              endOfInput
              return inps

-- | Parse a single line of CSV data.
parseCSVLine :: Parser InputLine
parseCSVLine = do event <- double
                  char ','
                  censored <- decimal
                  features <- many parseCSVFeature
                  endOfLine'
                  return (InputLine event (censored /= 0) (map (fst &&& fromJust . snd) $ filter (isJust . snd) (zip [1..] features)))

-- | Parse a single feature from CSV data.
parseCSVFeature :: Parser (Maybe Double)
parseCSVFeature =
  do char ','
     (do value <- double; return (Just value)) <|> (skipToEndOfLine *> return Nothing)

-- | Newline parser to handle all sorts of horrible.
endOfLine' :: Parser ()
endOfLine' = endOfLine <|> endOfInput <|> (char '\r' *> return ())

-- | Skip to the next line...
skipToEndOfLine :: Parser ()
skipToEndOfLine = takeWhile (\c -> c /= '\n' && c /= '\r') >> endOfLine'
