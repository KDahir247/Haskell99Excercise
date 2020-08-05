module PartOne(lastElement,
                butLast,
                elementAt,
                length',
                reverse',
                isParlindrome,
                NestedList(..),
                flatten,
                compress,
                pack,
                encode) where

import Data.List
--1 Find the last element of a list.
lastElement::(Show a) => [a] ->  a --Any variable the derives from Show
lastElement  = foldl1(\acc x -> x) 


--2 Find the last but one element of a list.
butLast ::(Show a) => [a] -> a ----Any variable the derives from Show
butLast (x:xs) 
    | length xs > 1 = butLast xs
    | otherwise = x

--3 Find the K'th element of a list. The first element in the list is number 1.
elementAt :: (Show a) => [a] -> Int -> a
elementAt xs a = xs !! (a -1) -- offset since list start at 0
elementAt (x:_) _ = x
elementAt _ a = error "List not recognized"


--4 Find the number of elements of a list.
length'::(Show a) => [a] -> Int
length' = foldr(\x acc -> 1 + acc)0 


--5 Reverse a list.
reverse'::(Eq a) => [a] -> [a]
reverse' = foldl(\acc x -> x:acc)[] 

--6 Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isParlindrome::(Eq a) => [a] -> Bool
isParlindrome xs 
    | xs == revxs = True
    | otherwise = False
    where revxs = reverse' xs

--7 Flatten a nested list structure.
--Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).
data NestedList a = Elem a | List [NestedList a]

flatten (Elem x) = [x]
flatten (List x) = concatMap flatten x

--8 Eliminate consecutive duplicates of list elements.
--If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
compress::(Ord a) => [a] -> [a]
compress = foldr(\x acc -> if not(elem x acc) then sort (x : acc) else acc)[] 


-- 9 Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
pack ::(Ord a) => [a] -> [[a]]
pack xs =  groupBy (==) (sort xs)


-- 10 Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
encode xs = map (\x -> (length x , head x)) (group xs)
