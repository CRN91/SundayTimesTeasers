{- 
Sunday Times Teaser 3158 https://www.thetimes.co.uk/article/teaser-3158-wwgkh9d99

“I have a couple of subtraction problems for you”, George told Martha.

Look: N1 – N2 = N3 and N3 – N4 = N5.

Can you solve them if I tell you that N1, N3 and N5 are all three-digit whole numbers 
whose sum is less than 2000, the same three non-zero digits appearing in all three
numbers but no digit being repeated within any of those numbers? N2 and N4 are both 
two-digit whole numbers using two of the three digits mentioned above, and the first 
digit of N1 is not equal to the first digit of N2.

What is N1?
 -}

import Data.List

{-Converts an non zero integer to a list of its digits-}
digits :: Int -> [Int]
digits 0
  = []
digits x -- Recursively adds the last digit of x to the list whilst removing it from the integer
  = digits (div x 10) ++ mod x 10 : [] :: [Int] 

{-Converts a list of digits into a single integer-}
number :: [Int] -> Int
number [] 
  = 0
number x -- Recursively appends the last digit in the list to the returned number 
  = last x + 10 * number (init x)

{- Returns True if all elements in the list are unique and not 0 else False -}
isSet :: [Int] -> Bool
isSet []
  = True
isSet (x:xs) -- Recursively checks if x is 0 or is in the list xs
  | x == 0 = False
  | elem x xs = False
  | otherwise = isSet xs

{-Generates a list of 'digits' length lists, of which each element is a permutation of 1 to 9-}
generatePermutations :: Int -> [[Int]]
generatePermutations n -- Generates all n length numbers as integers and then converts to lists
  = map digits [(10^n)-9*10^(n-1)..(10^n-1)]

{-Generates a list of lists, of which each element is unique and non zero-}
uniqueDigits :: Int -> [[Int]]
uniqueDigits n -- Filters n permutations with isSet
  = filter isSet(generatePermutations n)
  
{-Returns True if the first list is a subset of the second list-}
isSubset :: [Int] -> [Int] -> Bool
isSubset [] y
  = True
isSubset (x:xs) y -- Recursively checks if elements of the first list are in the second
  | elem x y = isSubset xs y
  | otherwise = False

{-Returns a list of tuples that are possible solutions to Teaser 3158-}
possibles :: [([Int], [Int], [Int], [Int], [Int])]
possibles               -- n1 is a 3 digit number where each digit is unique and non zero
  = [(n1,n2,n3,n4,n5) | n1 <- uniqueDigits 3,
                        -- n2 is a 2 digit number that is a subset of n1 and whose first digit is different from n1's 
                        n2 <- filter (\x -> head x /= head n1) (filter (\xs -> isSubset xs n1) (uniqueDigits 2)),
                        -- n3 is a 3 digit combination of n1's elements
                        n3 <- filter (\x -> sort x == sort n1) (uniqueDigits 3),
                        -- n4 is a 2 digit number that is a subset of n1
                        n4 <- filter (\xs -> isSubset xs n1) (uniqueDigits 2),
                        -- n5 is a 3 digit combination of n1's elements
                        n5 <- filter (\x -> sort x == sort n1) (uniqueDigits 3)
    ]

{-Takes a tuple of 5 integer lists of digits and converts it into a list of 5 numbers-}
teaserTupleToNumbers :: ([Int], [Int], [Int], [Int], [Int]) -> [Int]
teaserTupleToNumbers (n1,n2,n3,n4,n5)
  = map number [n1,n2,n3,n4,n5]

{-Takes a list of integers and checks they satisfy the numeracy related requirements for Teaser 3158-}
isNumeracyConditions :: [Int] -> Bool
isNumeracyConditions [n1,n2,n3,n4,n5]
  = n1 - n2 == n3 && n3 - n4 == n5 && n1 + n3 + n5 < 2000

{-Checks the tuple fits all non numeracy related requirements for Teaser 3158-}
isOtherConditions :: ([Int], [Int], [Int], [Int], [Int]) -> Bool
isOtherConditions (n1,n2,n3,n4,n5)
  = head n1 /= head n2 && sort n1 == sort n3 && sort n1 == sort n5 && sort n2 == sort n4 && isSubset n2 n1 && all isSet [n1,n2,n3,n4,n5]

{-Checks the given tuple adheres to every single condition to satisfy Teaser 3158-}
isAcceptable :: ([Int], [Int], [Int], [Int], [Int]) -> Bool
isAcceptable xs
  = isNumeracyConditions (teaserTupleToNumbers xs) && isOtherConditions xs
  
main = do
  print (filter isAcceptable possibles)

{-Tests-}
  --print( digits 9124 == [9,1,2,4] )
  --print( number [9,1,2,4] == 9124 )

  --print (length possibles == 435456)

  --print (filter isAcceptable possibles)