-- File    : Proj2.hs
-- Author  : Tom Sinclair
-- ID      : 1350315
-- Purpose : 
--    A program to generate all possible haikus given a list of words. 
--    All haikus have no repeated words.
-- Desc    :
    {-
        This file primarily concerns functions fillInPoem and generateAllHaikus.
        fillInPoem creates a poem given a wordlist and an order of syllables,
        and generateAllHaikus uses this to generate all possible haikus given
        a wordlist. Poems do not have any repeated words.
        
        A haiku is represented as a list of strings [s_1, s_2, ..., s_n],
        where n >= 1. A haiku must, for:
        - s_1...s_i have a cumulative syllable count of 5, for i ∈ [1..n-2].
        - s_i...s_k have a cumulative syllable count of 7, for k ∈ [i..n-1].
        - s_k...s_n have a cumulative syllable count of 5.
    -}

module Proj2 
  (fillInPoem, generateAllHaikus)
where

import ProblemSetup (Poem, PoemMetric(..), PoemScore)
import Words (syllables)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.List as List
import Data.Maybe (fromJust)

type Counts a = M.Map a Int
type Partition = [Int]

type SyllableOrder = Partition
    -- a SyllableOrder is a list of ordered integers where
    -- each integer represents the number of syllables of a
    -- word. Each integer lines up with its corresponding word
    -- in a (implied) list of strings.


{-| fillInPoem wordlist wordSyllables
    
    takes a list of words `wordlist` and 
    a SyllableOrder `wordSyllables`, 
    and creates all possible poems from `wordlist` 
    which correspond to SyllableOrder.
    
    All poems generated do not have any repeated words.
-}
fillInPoem :: [String] -> SyllableOrder -> [Poem]
fillInPoem _ [] = [[]]
fillInPoem wordlist wordSyllables = 
    -- bucket words by syllable in a Map
    let buckets = M.fromListWith (++) [(syllables w, [w]) | w <- wordlist] in
    
    -- match words with order in wordSyllables, ready for haiku construction
    let bucketsLists = 
         [M.findWithDefault [] (Just s) buckets | s <- wordSyllables] in
    
    constructGoodPoems bucketsLists Set.empty
    
    
{-| constructGoodPoems buckets accumulated
    
    takes a list of a list of Strings `buckets`,
    where each list is a bucket of strings grouped by 
    equal syllable counts, and a set of strings `accumulated`.
    
    It recursively constructs the cartesian product 
    of the buckets, i.e.
        buckets [["word1", "word2"] ["word3"]] become 
        poems [["word1", "word3"], ["word2", word3"]],
    whilst ensuring the resulting poems do not contain any repeated words.
    
    It is likely that the initial call to this function 
    should pass through an empty set as the set of the strings, 
    although if any word should strictly not be in any of the 
    output poems, this could be a member of the initial set.
-}
constructGoodPoems :: [[String]] -> Set String -> [Poem]
constructGoodPoems [] _ = [[]]
constructGoodPoems (b:bs) accumulated =  
    [word:cartesian | word <- b, word `Set.notMember` accumulated, 
        cartesian <- constructGoodPoems bs (Set.insert word accumulated)]


{-| getValidPartitions n counts
    
    recursively gathers the partitions of Int `n`,
    constructed only from the keys of Map
    `counts`. 
    
    `counts` is a Counts that represents 
    how many uses of a number is still 
    available for each partition. 
    
    This function outputs a list of 
    each partition is paired with its 
    corresponding `counts` (after construction).
    
    For the initial call, `counts` should be
    the total available for each element.
-}
getValidPartitions :: Int -> Counts Int -> [(Partition, Counts Int)]
getValidPartitions n counts 
    | n == 0 = [([], counts)] -- found a partition! 
    | n < 0 = [] -- went over, invalid partition.
    | otherwise = 
    
        -- only consider partitions with syllables we actually have
        concatMap go (M.keys counts)
    where go x =
            let count = M.findWithDefault 0 x counts in 
                -- theres a word available to use...
                if (count > 0) then
                
                    -- decrement #available words  syllable count
                    let updatedMap = M.adjust (\v -> v - 1) x counts in
                    
                    -- a valid partition if x, with prev partitions, sum to n
                    let prev = getValidPartitions (n - x) updatedMap in
                        map (\(p, m) -> (x:p, m)) prev
                             
                -- no word available to use for this syllable; invalid
                else
                   [] 
                   
                   
{-| generateAllHaikus wordlist
    
    generates a list of all possible distinct haikus 
    made up of words from the string of words `wordlist`. 
    Each haiku does not contain any repeated words.   
-}
generateAllHaikus :: [String] -> [Poem]
generateAllHaikus [] = [[]]
generateAllHaikus wordlist = 
    -- count how many words we can use, according to their #syllables
    let syllableList = [fromJust (syllables w) | w <- wordlist] in
    let syllableCounts = countElems syllableList in
    
    let orders1and3 = getValidPartitions line1and3n syllableCounts
        orders2 = getValidPartitions line2n syllableCounts in 
    
    let haikuPartitions = 
         constructHaikuOrders 
             orders1and3 orders2 syllableCounts in
        
        -- each partition could have multiple possible poems, 
        -- so we have to concat
        concatMap (fillInPoem wordlist) haikuPartitions


{-| constructHaikuMaps a-syllables b-syllables totalSyllableOrders
     
     
     constructs every combination of syllableOrders from 
     `a-syllables`, `b-syllables`, `a-syllables`.
     No combination contains symbolized words that are not
     actually available from original wordlist, where 
     `totalSyllableOrders`is the total counts of each 
     syllable available.
     
     `a-syllables` and `b-syllables` are a list 
     of (Partition, Count) pairs, likely created 
     from function getValidPartitions.
     Requires 'Count' represent the words of 
     certain syllable counts *still available* 
     (rather than words used).
    
     This is only specific to poems of form 
     syllable counts a-b-a.
-}
constructHaikuOrders :: [(SyllableOrder, Counts Int)] -> 
                        [(SyllableOrder, Counts Int)] -> 
                        Counts Int -> [SyllableOrder]
 
constructHaikuOrders fives sevens syllableOrders =
    -- the maps across 'fives' and 'sevens' represent how many syllables
    -- they have *remaining*. To count across the whole poem, we need to
    -- be able to see how many syllables they've used:
    let convertToCount = 
         map (\(p,k) -> (p, M.unionWith (-) syllableOrders k)) in

        -- cartesian product of lines, 
        -- ensuring distinctness whilst constructing
        [concat [l1, l2, l3] | 
            -- don't need to convert first line map since
            -- its already whats remaining after the first line
            (l1, m1) <- fives, 

            -- remaining after 2 lines...
            (l2, m2) <- (convertToCount sevens),
            let map2lines = M.unionWith (-) m1 m2,

            -- still words to use, keep going
            all (>=0) (M.elems map2lines), 

            -- remaining after 3 lines...
            (l3, m3) <- (convertToCount fives),
            let map3lines = M.unionWith (-) map2lines m3,

            -- successful haiku!
            all (>=0) (M.elems map3lines)
            ]
       
       
{-| countElems list Counts
    
     traverses `list` and creates a Map
     with each key-value pair representing a
     distinct element and its corresponding count.

     from https://tinyurl.com/epwvk5yd
-}
countElems :: (Ord a) => [a] -> Counts a
countElems = M.fromListWith (+) . flip zip (repeat 1)
    

-- MAGIC NUMBERS

line1and3n :: Int
line1and3n = 5

line2n :: Int
line2n = 7

   

    


    
    
    
    