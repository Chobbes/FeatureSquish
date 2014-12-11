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

import FeatureSquish.InputLine
import FeatureSquish.Parser

import Control.Arrow

import Data.Attoparsec.Text hiding (take)
import Data.List
import Data.List.Split hiding (split)
import Data.Maybe
import qualified Data.Text.IO as T

import System.Directory
import System.Environment
import System.FilePath
import System.Random


-- | FeatureSquish dataset output_directory iterations prob_of_removal+
main :: IO ()
main = do (file : testProbStr :outDir : testDir : csvDir : testCSVDir : iterStr : probStrs) <- getArgs

          let iterations = read iterStr
          let probs = map read probStrs
          let testProb = read testProbStr

          let fileName = takeFileName file
          let baseName = dropExtensions fileName
          let extension = takeExtensions fileName

          fileData <- T.readFile file
          let inp = takeRight $ parseOnly parsePSSP fileData
                where takeRight (Right a) = a
                      takeRight _ = error "Could not parse file"

          gen <- getStdGen

          putStrLn "Writing files..."
          createDirectoryIfMissing True outDir
          mapM_ (writeRun outDir testDir baseName extension . (\(p,g) -> (p, squishMultiple iterations inp testProb p g))) (zip probs (splits gen))
          mapM_ (writeRunCSV csvDir testCSVDir baseName extension . (\(p,g) -> (p, squishMultiple iterations inp testProb p g))) (zip probs (splits gen))
          putStrLn "Done!"

-- | Write all iterations for a given probability to a file
writeRun :: FilePath -> FilePath -> String -> String -> (Double, [([InputLine], [InputLine])]) -> IO [()]
writeRun outDir testDir baseName extension (prob, inps) = 
  do mapM_ (writeIteration outDir baseName extension) (zip [1..] (zip (repeat prob) trainInputs))
     mapM (writeIteration testDir baseName extension) (zip [1..] (zip (repeat prob) testInputs))
  where trainInputs = map fst inps
        testInputs = map snd inps

-- | Write all iterations for a given probability to a CSV file
writeRunCSV :: FilePath -> FilePath -> String -> String -> (Double, [([InputLine], [InputLine])]) -> IO [()]
writeRunCSV outDir testDir baseName extension (prob, inps) =
  do mapM_ (writeIterationCSV outDir baseName extension) (zip [1..] (zip (repeat prob) trainInputs))
     mapM (writeIterationCSV testDir baseName extension) (zip [1..] (zip (repeat prob) testInputs))
  where trainInputs = map fst inps
        testInputs = map snd inps
        
-- | Write a single iteration to a file.
writeIteration :: FilePath -> String -> String -> (Integer, (Double, [InputLine])) -> IO ()
writeIteration outDir baseName extension (iter, (prob, inp)) = 
  do createDirectoryIfMissing True probDir
     writeFile iterFile (intercalate "\n" $ map show inp)
  where probDir = joinPath [outDir, show prob, show iter]
        iterFile = joinPath [probDir, baseName ++ extension]

-- | Write a single iteration to a CSV file.
writeIterationCSV :: FilePath -> String -> String -> (Integer, (Double, [InputLine])) -> IO ()
writeIterationCSV outDir baseName extension (iter, (prob, inp)) = 
  do createDirectoryIfMissing True probDir
     writeFile iterFile (linesToCSV inp)
  where probDir = joinPath [outDir, show prob, show iter]
        iterFile = joinPath [probDir, baseName ++ extension]

-- | Generate several squished versions of the data.
squishMultiple :: RandomGen g => Int -> [InputLine] -> Double -> Double -> g -> [([InputLine], [InputLine])]
squishMultiple iterations inp testProb prob gen = take iterations $ map (squishList inp testProb prob) (splits gen)

-- | Remove features with a given probability from an InputLine list.
squishList :: RandomGen g => [InputLine] -> Double -> Double -> g -> ([InputLine], [InputLine])
squishList inp testProb prob gen = (zipWith (squish prob) (splits splitGen) $ map snd trainPart, map snd testPart)
           where (splitGen, testGen) = split gen
                 (testPart, trainPart) = partition (\(p,_) -> p <= prob) (zip (randomRs (0.0, 1.0) testGen) inp)

-- | Remove features with a given probability from an InputLine.
squish :: RandomGen g => Double -> g -> InputLine -> InputLine
squish prob gen inp = inp {features = [f | (x, f) <- zip missing feats, x == 1]}
       where missing = map (\x -> if x > prob then 1 else 0) (randomRs (0.0, 1.0) gen)
             feats = features inp

-- | Create an infinite list of random generators.
splits :: RandomGen a => a -> [a]
splits g = gh : splits gt
       where (gh, gt) = split g
