module Main where

import Debug.Trace

import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified System.Random as R

data Lexeme = LexemeString String
            | LexemeEnd
  deriving(Show, Eq)

type CountMap = M.Map String [(Lexeme, Int)]

updateAssocList :: (Eq k) => (k, v) -> [(k, v)] -> [(k, v)]
updateAssocList (k, v) assoc =
  case index of
    Just i  -> (take i assoc) ++ ((k, v):(drop (i + 1) assoc))
    Nothing -> (k, v):assoc
  where index = findIndex ((==) k . fst) assoc

buildCountMap :: [String] -> CountMap
buildCountMap wordList = builder M.empty wordList
  where relations word wordMap =
          fromMaybe [] $ M.lookup word wordMap
        builder wordMap [] = wordMap
        builder wordMap (last:[]) =
          let rels = relations last wordMap
              newRel = (LexemeEnd, 1)
          in M.insert last (newRel:rels) wordMap
        builder wordMap (first:second:wordList) =
          let rels = relations first wordMap
              count = fromMaybe 0 $ lookup (LexemeString second) rels
              newRel = (LexemeString second, count + 1)
              newRels = updateAssocList newRel rels
          in builder (M.insert first newRels wordMap)
                     (second:wordList)

type FrequencyMap = M.Map String [(Lexeme, Float)]

convertCountAssocToFrequencyAssoc :: (Eq a) => [(a, Int)] -> [(a, Float)]
convertCountAssocToFrequencyAssoc assoc = transform [] assoc
  where totalCount = foldl' (\a b -> a + (snd b)) 0 assoc
        transform newList [] = newList
        transform newList ((k, v):rest) =
          let freq = (fromIntegral v) / (fromIntegral totalCount)
          in transform ((k, freq):newList) rest

buildFrequencyMap :: CountMap -> FrequencyMap
buildFrequencyMap = M.mapWithKey convert
  where convert _ = convertCountAssocToFrequencyAssoc

readInput :: String -> FrequencyMap
readInput = buildFrequencyMap . buildCountMap . map (map toLower) . words

pickWord :: (Eq a) => Float -> [(a, Float)] -> a
pickWord _ [] = error "Walking the assoc list somehow didn't result in a word."
pickWord t ((word, freq):assoc)
  | t <= freq = word
  | otherwise = pickWord (t - freq) assoc

generateWords :: FrequencyMap -> Lexeme -> Int -> IO [String]
generateWords _ _ 0 = return []
generateWords freqMap LexemeEnd limit = return []
generateWords freqMap (LexemeString seed) limit = do
  randVal <- (R.getStdRandom R.random) :: (IO Float)
  word <- return $ pickWord randVal assocList
  remainingWords <- generateWords freqMap word (limit - 1)
  return (seed:remainingWords)
  where assocList = fromMaybe (error $ "Seed word '" ++ seed ++ "' not in map")
                    $ M.lookup seed freqMap

main :: IO ()
main = do
  input <- getContents
  freqMap <- return $ readInput input
  seed <- return $ LexemeString $ map toLower $ head $ words input
  generated <- generateWords freqMap seed 100
  putStrLn $ unwords generated
