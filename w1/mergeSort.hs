import Data.Char

data SortedList = SortedList {
	inversionCount :: Int,
	list :: [Int]
} deriving (Show) 

--		first	list 		accumulator
packm :: Int -> SortedList -> Int -> SortedList
packm x (SortedList count xs) add =  SortedList (count + add) (x:xs)

merge2 :: [Int] -> [Int] -> SortedList
merge2 [] xs = SortedList 0 xs
merge2 xs [] = SortedList 0 xs
merge2 xlist@(x:xs) ylist@(y:ys)
		| x < y = packm x (merge2 xs ylist) 0
		| otherwise = packm y (merge2 xlist ys) $ length xlist



countAndMerge :: SortedList -> SortedList -> SortedList
countAndMerge (SortedList lcount lxs) (SortedList rcount rxs) = 
	let 
		merged = merge2 lxs rxs
	in SortedList (lcount + rcount + inversionCount merged) $ list merged

mergesort :: [Int] -> SortedList
mergesort [] = SortedList 0 []
mergesort [x] = SortedList 0 [x]
mergesort xs =	let 	
					leftsorted = mergesort $ take halfElements xs
					rightsorted = mergesort $ drop halfElements xs
				in countAndMerge leftsorted rightsorted
				where halfElements = length xs `div` 2

main = do 
	contents <- getContents
	let intlist = [ read x :: Int | x <- (lines contents) ]
	print $ inversionCount $ mergesort intlist

