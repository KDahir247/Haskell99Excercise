module PartTwo(encodeModified,
                decodeModified,
                decodeCalc,
                encodeDirect,
                duplicate,
                repli,
                mydrop,
                split',
                slice,
                rotate,
                removeAt',
                removedelete) where
import Data.List

--11 Modified run-length encoding.
--Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.
data Occurence a = Single | Multiple a
    deriving(Show, Eq)

encodeModified :: (Ord b) => [b] -> [(Occurence Int, b)]
encodeModified xs = map (\x -> (if length x <=1 then Single else Multiple (length x) , head x)) (group $ sort xs)

--12 Decode a run-length encoded list.
--Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.
decodeModified :: [(Occurence Int, a)] -> [a]
decodeModified [] = []
decodeModified xs = decodeCalc (fst(head xs)) (snd(head xs)) ++ decodeModified (tail xs)

decodeCalc :: Occurence Int -> a -> [a]
decodeCalc Single x = [x]
decodeCalc (Multiple a) x = take a (repeat x)

--13 Run-length encoding of a list (direct solution).
--Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.
encodeDirect :: (Ord b) => [b] -> [(Occurence Int, b)]
encodeDirect xs = map (\x -> (if length x <=1 then Single else Multiple (length x) , head x)) (group xs)

--14 Duplicate the elements of a list.
duplicate :: Ord a => [a] -> [a]
duplicate xs = sort (take (length xs *2) $ cycle xs)

--15 Replicate the elements of a list a given number of times.
repli :: (Ord a) => [a] -> Int -> [a]
repli xs v = sort $ take (length xs * v) $ cycle xs

--16 Drop every N'th element from a list.
mydrop::(Eq a) => [a] -> Int ->[a]
mydrop xs v = foldl(\acc x -> if length acc < v then acc++[x] else acc)[] xs

--17 Split a list into two parts; the length of the first part is given.
split' :: [a] -> Int -> ([a], [a])
split' xs v  = splitAt v xs

--18 Extract a slice from a list.
--Given two indices, i and k, the slice is the list containing the elements between the i'th and k'th element of the original list (both limits included). Start counting the elements with 1.
slice::[a] -> Int -> Int -> [a]
slice xs start end =  take (end - start +1) (drop (start-1) xs) --offset for remap list start value from 0 to 1

--19 Rotate a list N places to the left.
rotate:: [a] -> Int -> [a]
rotate xs v = slice xs (v + 1) (length xs) ++ take(v) xs --doesn't support negative number

--20 Remove the K'th element from a list.
removeAt' :: Eq a => Int -> [a] -> (a, [a])
removeAt' i xs  = (xs!!(i-1), removedelete(xs!!(i -1)) xs)

removedelete :: (Foldable t, Eq a) => a -> t a -> [a]
removedelete i xs = foldr(\x acc -> if x == i then acc else x : acc )[] xs