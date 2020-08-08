module PartThree (insertAt,
                    range,) where 

import System.Random
import Data.List
--21 Insert an element at a given position into a list.
insertAt ::(Foldable t) => a -> t a -> Int -> [a]
insertAt v xs i = foldr(\x acc ->  if length acc == i then v : x: acc else x : acc )[] xs

--22 Create a list containing all integers within a given range.
range:: Int -> Int -> [Int]
range start end = take (end - start + 1) [start..end] -- offset by one since list remapping starting from zero to one

--23 Extract a given number of randomly selected elements from a list.

--24 Draw N different random numbers from the set 1..M.

--25 Generate a random permutation of the elements of a list.

--26 Generate the combinations of K distinct objects chosen from the N elements of a list

--27 Group the elements of a set into disjoint subsets.

--28 Sorting a list of lists according to length of sublists
lsortOrd :: (Foldable t1, Foldable t2) => t1 a1 -> t2 a2 -> Ordering
lsortOrd lft rght 
    | length lft > length rght = GT
    | otherwise = LT

lsort :: (Ord a) => [[a]] -> [[a]]
lsort xs = sortBy lsortOrd xs
