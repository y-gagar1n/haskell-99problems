import System.Random

myLast :: [a] -> a
myLast = myHead . myReverse

myHead :: [a] -> a
myHead (x:_) = x

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myButLast :: [a] -> a
myButLast [] = error "empty list"
myButLast [_] = error "only one element"
myButLast (x:_:[]) = x
myButLast (_:xs) = myButLast xs

elementAt :: (Ord i, Num i) => [a] -> i -> a
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (x:xs) = (x == myLast xs) && (isPalindrome $ init xs)

data NestedList a = Elem a | List [NestedList a]

--flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
--[1,2,3,4,5]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten(List xs)


--compress "aaaabccaadeeee"
--"abcade"
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:y:xs) 
	| x == y = compress(x:xs)
	| otherwise = [x] ++ compress(y:xs)

--pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
--["aaaa","b","cc","aa","d","eeee"]
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = [packAcc x [x] xs] ++ pack(dropWhile (==x) xs)
	where 
		packAcc _ acc [] = acc
		packAcc x acc (y:ys)
			| x == y = packAcc x (acc++[y]) ys
			| otherwise = acc

--encode "aaaabccaadeeee"
--[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode x = map getLength (pack x)
	where getLength x = (myLength x, head x)

--encodeModified "aaaabccaadeeee"
--[Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
data Several a = Multiple Int a | Single a deriving Show

encodeModified :: (Eq a) => [a] -> [Several a]
encodeModified [] = []
encodeModified x = map getLength (pack x)
	where getLength x = let l = myLength x; h = head x in (if l > 1 then Multiple l h else Single h)

-- decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"
decodeModified :: (Eq a) => [Several a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = (decodeSeveral x) ++ (decodeModified xs)
	where 
		decodeSeveral (Single x) = [x]
		decodeSeveral (Multiple n x) = take n (repeat x)


--encodeDirect "aaaabccaadeeee"
--[Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
encodeDirect :: (Eq a) => [a] -> [Several a]
encodeDirect [] = []
encodeDirect x = map convert $ foldr helper [] x
	where 
		helper x [] = [(1,x)]
		helper x (y@(a,b):ys)
			| x == b = (a+1,b):ys
			| otherwise = (1,x):y:ys
		convert (1,x) = Single x
		convert (n, x) = Multiple n x

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:(dupli xs)

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (take n $ repeat x) ++ (repli xs n)

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery x n = dropHelper n 1 x
	where
		dropHelper _ _ [] = []
		dropHelper n i (x:xs)
			| n == i = dropHelper n 1 xs
			| otherwise = x:(dropHelper n (i+1) xs)

remove_at :: Int -> [a] -> [a]
remove_at n x = remove_at' x n 1
	where
		remove_at' y@(x:xs) n i			
			| i >= n = xs
			| otherwise = x:(remove_at' xs n (i+1))

insert_at :: a -> [a] -> Int -> [a]
insert_at c x n = insert_at' c x n 1
	where
		insert_at' c y@(x:xs) n i
			| i >= n = c:y
			| otherwise = x:(insert_at' c xs n (i+1))

range :: Int -> Int -> [Int]
range a b = if a == b then [b] else a : range (a + 1) b

rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
	gen <- getStdGen
	return $ take n $ [xs !! x | x <- randomRs (0, length xs - 1) gen]

diff_select :: Int -> Int -> IO [Int]
diff_select k n = diff_select' k [1..n]
		
diff_select' 0 _ = return []
diff_select' _ [] = return []
diff_select' k xs = do
	r <- randomRIO (0, (length xs) - 1)
	let remaining = take r xs ++ drop (r+1) xs
	rest <- diff_select' (k-1) remaining
	return ((xs!!r) : rest)

rnd_permu :: [a] -> IO [a]
rnd_permu xs = diff_select' (length xs) xs

is_prime :: Int -> Bool
is_prime 0 = False
is_prime 1 = False
is_prime x = is_prime' x 2
	where 
		is_prime' x i
			| i == x = True
			| i < x = if x `mod` i == 0 then False else is_prime' x (i+1)
			|otherwise = error "something went wrong"

main = putStrLn $ show $ is_prime 12
