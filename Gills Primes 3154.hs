{- The Sunday Times Teaser 3154
Gill’s Primes

Jack told Gill “I have found three equally-spaced prime numbers 29, 41, and 53.
The difference between the first and second is the same as the difference between
the second and third, and there are no repeated digits in the six digits of my primes”.
Gill told Jack she had also found three equally-spaced primes, each having three
digits and with no repeated digits in the nine digits of her primes. She said,
“If Itold you that the three-digit sum of each of my primes is an odd number then
you should be able to find them”.

In ascending order what are Gill’s three primes?
-}

import Data.List

{- Returns True if an integer is a prime number-}
isPrime :: Int -> Bool
isPrime 1
  = False
isPrime 2
  = True
isPrime x
  = length (filter (\y -> mod x y == 0) [2..(x-1)]) == 0 

{- Converts an integer to a list of its digits -}
intToList :: Int -> [Int]
intToList 0
  = []
intToList x
  = intToList (div x 10) ++ mod x 10 : [] :: [Int]

{- Converts a list of integers into a single integer where each element of the list represents a digit -}
listToInt :: [Int] -> Int
listToInt [] 
  = 0
listToInt x
  = last x + 10 * listToInt (init x)

{- Converts an integer to a list of 0s followed by its digits to make its length 3 -}
littleIntToList :: Int -> [Int]
littleIntToList x
  | x > 9 = [0] ++ intToList x
  | otherwise = [0,0] ++ intToList x

{- Generates a list of lists of length digits, where each element is a permutation of 1 to 9 -}
generatePermutations :: Int -> [[Int]]
generatePermutations digits
  = map littleIntToList [1..99] ++ map intToList [(10^digits)-9*10^(digits-1)..(10^digits-1)]

{- Returns True if all elements in the list are unique else False -}
isSet :: [Int] -> Bool
isSet []
  = True
isSet (x:xs)
  | elem x xs = False
  | otherwise = isSet xs

{- Filters digits permutations with isSet to generate a list of unique lists where no digit is repeated or 0 -}
uniqueDigits :: Int -> [[Int]]
uniqueDigits digits
  = filter isSet(generatePermutations digits)
  
{- Takes 2 lists and returns True if none of the elements of the second list are in the first-}
uniqueLists :: [Int] -> [Int] -> Bool
uniqueLists x []
  = True
uniqueLists x (y:ys)
  | elem y x = False
  | otherwise = uniqueLists x ys

{- Returns a list of tuples that are possible solutions to Teaser 3154-}
possibles :: [([Int], [Int], [Int])]
possibles         -- n1 is a 3 digit number where each digit is unique
  = [(n1,n2,n3) | n1 <- uniqueDigits 3,
                  -- n2 is a 3 digit number that contains none of the digits of n1
                  n2 <- filter (\xs -> uniqueLists n1 xs) (uniqueDigits 3),
                  -- n3 is a 3 digit number that contains none of the digits of n1 and n2
                  n3 <- filter (\xs -> uniqueLists (n1 ++ n2) xs) (uniqueDigits 3)
    ]

{- Returns True if all elements of a list are unique-}
isUnique :: [Int] -> Bool
isUnique []
  = True
isUnique (x:xs)
  | elem x xs = False
  | otherwise = isUnique xs

{-Returns True if the difference between the 1st and 2nd numbers is the same as the difference between the 2nd and 3rd-}
isSameDifference :: [Int] -> Bool
isSameDifference [n1,n2,n3]
  = (n1-n2) == (n2-n3)

{-Checks if the sum of the 3 digits is odd-}
sumOf3DigitsIsOdd :: [Int] -> Bool
sumOf3DigitsIsOdd [x,y,z]
  = mod (x+y+z) 2 /= 0
sumOf3DigitsIsOdd x 
  = False

{-Returns True if for all numbers in the list the sum of their digits is odd-}
allSumsOdd :: [Int] -> Bool
allSumsOdd []
  = True
allSumsOdd(x:xs)
  | sumOf3DigitsIsOdd (intToList x) = allSumsOdd xs
  | otherwise = False

{-Returns True if all conditions of Teaser 3154 are met-}
isTeaser :: [Int] -> Bool
isTeaser xs
  = isUnique xs && isSameDifference xs && all isPrime xs && allSumsOdd xs

{-Converts the tuple to a list of integers-}
teaserTupleToNumbers :: ([Int], [Int], [Int]) -> [Int]
teaserTupleToNumbers (n1,n2,n3)
  = map listToInt [n1,n2,n3]

{-Returns True if the tuple is a solution to Teaser 3154-}
isAcceptable :: ([Int], [Int], [Int]) -> Bool
isAcceptable xs
  = isTeaser (teaserTupleToNumbers xs)

main = do
  print(filter isAcceptable possibles)

{-Tests-}
  --print(isPrime 293 == True)
  --print(isPrime 295 == False)
  
  --print(length possibles == 3628800)
  
  --print((isAcceptable ([2,0,1],[3,4,5],[6,7,8])) == False)