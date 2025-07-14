--  File   : ProblemSetup.hs
--  Author : Vincent Jackson
--  Purpose: Basic definitions for Project 2: Algorithmic Poet

module ProblemSetup
  (distinct, Poem, PoemMetric(..), PoemScore, isGoodHaiku)
where

import Data.List (elem)
import Data.Maybe (fromJust)

import Words (syllables)

-- tests if all elements in a list are distinct
distinct :: Eq a => [a] -> Bool
distinct [] = True
distinct (x:xs) = not (elem x xs) && distinct xs


-- A poem is a list of words
type Poem = [String]

-- A poem is scored by a number, possibly fractional
type PoemScore = Double

-- There are three sorts of poem metrics:
-- Alliteration: the sum of the lengths of each *maximal alliterative runs*
--               in the poem.
-- Homogeneity: the number of synonymous word-pairs in the poem
-- Diversity: the number of non-synonymous word-pairs in the poem
data PoemMetric = Alliteration | Homogeneity | Diversity

-- Tests if a poem is a good Haiku (with respect to a particular wordlist).
-- Precondition: wordlist is a sublist of the big wordlist.
isGoodHaiku :: [String] -> Poem -> Bool
isGoodHaiku wordlist p = 
  -- a poem is a good haiku when:
  let -- every word comes from the wordlist
      wordlistCond = all (`elem` wordlist) p
      -- it has the correct amount of syllables per line
      scanSylls = scanl (\x w -> (+ x) . fromJust . syllables $ w) 0 p
      syllableCond =
        elem 5 scanSylls && elem 12 scanSylls && elem 17 scanSylls
      -- moreover, we require that all the words in the poem are distinct.
      wordDistinctCond = distinct p
  in wordlistCond && syllableCond && wordDistinctCond