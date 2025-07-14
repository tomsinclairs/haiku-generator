--  File   : Words.hs
--  Author : Mak Nazecic-Andrlon & Vincent Jackson
--  Purpose: Word utilities used for Project 2: Algorithmic Poet

module Words
  (syllables, wordList, wordToSyllablesMap)
  where

import qualified Data.ByteString as B
import qualified Data.Map.Strict as Map
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T
import Data.Bifunctor (second)
import Data.List (nub, sort)
import Data.Char (isAlpha, toUpper)
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

import Debug.Trace

--
-- Raw Data
--

{-# NOINLINE wordToSyllablesData #-}
wordToSyllablesData = T.decodeUtf8 $ unsafePerformIO $ B.readFile "word_to_syllables.txt"

--
-- Syllables
--

wordToSyllablesMap :: Map.Map T.Text (S.Set Int)
wordToSyllablesMap =
    Map.fromList
    . map (\line -> let words = T.words line
                    in (head words, S.fromList . map (read . T.unpack) . tail $ words))
    . T.lines
    $ wordToSyllablesData

syllables :: String -> Maybe Int
syllables =
  fmap (minimum . S.toList) . (`Map.lookup` wordToSyllablesMap) . T.toLower . T.pack

--
-- Word list
--

-- The list of all approved words.
wordList :: [String]
wordList = map T.unpack . Map.keys $ wordToSyllablesMap