--  File     : Main.hs
--  Author   : Vincent Jackson
--  Purpose  : Test program for proj2 project to be used in Grok

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Data.Bifunctor (first, bimap)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (intercalate)
import Data.Foldable (traverse_)

import ProblemSetup (Poem)
import Words (syllables)
import Proj2 (fillInPoem, generateAllHaikus)

--
-- Utils
--

-- Run the function on the argument, but keep the original value.
-- Puts the result in the snd position.
teeSnd :: (a -> b) -> a -> (a,b)
teeSnd f a = (a, f a)

-- A function that performs take and drop at the same time
takeDropWhile :: (a -> Bool) -> [a] -> ([a], [a])
takeDropWhile p = aux []
  where
    aux acc [] = (reverse acc, [])
    aux acc (x:xs) =
      if p x then
        aux (x:acc) xs
      else
        (reverse acc, x:xs)

-- scan through a list of tuples, summing the right components
sumScanlSnd :: forall a n. Num n => [(a, n)] -> [(a, n)]
sumScanlSnd = aux 0
  where
    aux :: n -> [(a, n)] -> [(a, n)]
    aux _ [] = []
    aux acc ((x,k) : xks) = aux (k+acc) ((x,k+acc) : xks)

-- Generate a pretty string representation of the given haiku, for printing to the console
prettyHaiku :: Poem -> String
prettyHaiku p =
  let pSyllAcc :: [(String, Int)]
      pSyllAcc = sumScanlSnd . map (teeSnd (fromMaybe 0 . syllables)) $ p
      (line1, rest1) = first (map fst) . takeDropWhile ((<= 5) . snd) $ pSyllAcc
      (line2, line3) = bimap (map fst) (map fst) . takeDropWhile ((<= 12) . snd) $ rest1
   in intercalate "\n" [unwords line1, unwords line2, unwords line3]

--
-- Main functionality
--

sampleWordlists =
  -- there should be 2 haikus with the following wordlist
  -- the second will be with the 5-syllable lines swapped
  [ ["abbreviation","abbreviator","americanisation"]
  -- there should be 1157760 haikus with the following wordlist
  , ["a","abb","able","ache","ace","abase","abash","aardvark","abacus","abalone"]
  ]

printRun :: Int -> IO ()
printRun k =
  do
    putStrLn $ "Your code found " ++ show k ++ " haikus"
    putStrLn ""
    return ()

main :: IO ()
main = traverse_ (printRun . length . generateAllHaikus) sampleWordlists