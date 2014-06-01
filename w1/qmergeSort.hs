mergesort :: [Int] -> [Int]
mergesort = mergesort' . map wrap

mergesort' :: [[Int]] -> [Int]
mergesort' [] = []
mergesort' [xs] = xs
mergesort' xss = mergesort' (merge_pairs xss)

merge_pairs :: [[Int]] -> [[Int]]
merge_pairs [] = []
merge_pairs [xs] = [xs]
merge_pairs (xs:ys:xss) = merge xs ys : merge_pairs xss

merge :: [Int] -> [Int] -> [Int]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys)
 = if x > y
        then y : merge (x:xs)  ys
        else x : merge  xs    (y:ys)

wrap :: Int -> [Int]
wrap x = [x]

main = do 
	contents <- getContents
	let intlist = [ read x :: Int | x <- (lines contents) ]
	print $ length $ mergesort intlist

