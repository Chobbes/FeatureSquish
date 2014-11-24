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

import Data.List
import Data.List.Split hiding (split)
import Data.Maybe
import System.Directory
import System.Environment
import System.FilePath
import System.Random

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

-- | FeatureSquish dataset output_directory iterations prob_of_removal+
main :: IO ()
main = do (file : outDir : iterStr : probStrs) <- getArgs

          let iterations = read iterStr
          let probs = map read probStrs

          let fileName = takeFileName file
          let baseName = dropExtensions fileName
          let extension = takeExtensions fileName

          fileData <- readFile file
          let inp = inputsRead fileData

          gen <- getStdGen

          putStrLn "Writing files..."
          createDirectoryIfMissing True outDir
          mapM_ (writeRun outDir baseName extension . (\(p,g) -> (p, squishMultiple iterations inp p g))) (zip probs (splits gen))
          putStrLn "Done!"




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
        maybeShow = maybe "" show


-- | Write all iterations for a given probability to a file
writeRun :: FilePath -> String -> String -> (Double, [[InputLine]]) -> IO [()]
writeRun outDir baseName extension (prob, inps) = 
  mapM (writeIteration outDir baseName extension) (zip [1..] (zip (repeat prob) inps))

-- | Write a single iteration to a file.
writeIteration :: FilePath -> String -> String -> (Integer, (Double, [InputLine])) -> IO ()
writeIteration outDir baseName extension (iter, (prob, inp)) = 
  do createDirectoryIfMissing True probDir
     writeFile iterFile (intercalate "\n" $ map show inp)
  where probDir = joinPath [outDir, show prob]
        iterFile = joinPath [probDir, baseName ++ "_" ++ show iter ++ extension]

-- | Read an entire dataset from http://pssp.srv.ualberta.ca/
inputsRead :: String -> [InputLine]
inputsRead = map lineRead . lines

-- | Read a single line from the http://pssp.srv.ualberta.ca/ dataset.
lineRead :: String -> InputLine
lineRead str = InputLine (read t) (read c /= 0) features
         where (t:c:featureStrs) = words str
               features = map readFeature featureStrs
               readFeature f = (read number, read value)
                 where [number, value] = splitOn ":" f
                 
-- | Generate several squished versions of the data.
squishMultiple :: RandomGen g => Int -> [InputLine] -> Double -> g -> [[InputLine]]
squishMultiple iterations inp prob gen = take iterations $ map (squishList inp prob) (splits gen)

-- | Remove features with a given probability from an InputLine list.
squishList :: RandomGen g => [InputLine] -> Double -> g -> [InputLine]
squishList inp prob gen = zipWith (squish prob) (splits gen) inp

-- | Remove features with a given probability from an InputLine.
squish :: RandomGen g => Double -> g -> InputLine -> InputLine
squish prob gen inp = inp {features = [f | (x, f) <- zip missing feats, x == 1]}
       where missing = map (\x -> if x > prob then 1 else 0) (randomRs (0.0, 1.0) gen)
             feats = features inp

-- | Create an infinite list of random generators.
splits :: RandomGen a => a -> [a]
splits g = gh : splits gt
       where (gh, gt) = split g
