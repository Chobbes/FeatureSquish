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
import Data.Attoparsec.Text
import Data.Maybe


-- | Parse PSSP data, either in CSV or MTLR format.
parsePSSP :: Parser [InputLine]
parsePSSP = try parseCSV <|> parseMTLR

-- | Parse a file of MTLR data.
parseMTLR :: Parser [InputLine]
parseMTLR = do inps <- many parseMTLR_Line
               endOfInput
               return inps

-- | Parse a single line of MTLR data.
parseMTLR_Line :: Parser InputLine
parseMTLR_Line = do time <- double
                    skipSpace
                    censored <- decimal
                    skipSpace
                    features <- many parseMTLR_Feature
                    endOfLine
                    return (InputLine time (censored /= 0) features)

-- | Parse a single feature:value pair for MTLR.
parseMTLR_Feature :: Parser (Integer, Double)
parseMTLR_Feature = do skipSpace
                       feature <- decimal
                       char ':'
                       value <- double
                       return (feature, value)

parseCSV :: Parser [InputLine]
parseCSV = do inps <- many parseCSV_Line
              endOfInput
              return inps

parseCSV_Line :: Parser InputLine
parseCSV_Line = do event <- double
                   char ','
                   censored <- decimal
                   features <- many parseCSV_Feature
                   endOfLine
                   return (InputLine event (censored /= 0) (zip [1..] $ catMaybes features))

parseCSV_Feature :: Parser (Maybe Double)
parseCSV_Feature = do char ','
                      try (do value <- double; return (Just value)) <|> do takeWhile1 (\c -> c /= ','); return Nothing
