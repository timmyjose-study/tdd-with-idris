import Data.Strings
import Data.List

{-
  1. 
    ("A", "B", "C") has the type (String, String, String)

    ["A", "B", "C"] has the type List String

    (('A', "B"), 'C') has the type ((Char, String), String)
-}

-- 2.

palindrome : (str : String) -> Bool
palindrome str = str == reverse str

-- 3.

palindrome' : (str : String) -> Bool
palindrome' str = toLower str == toLower (reverse str)

-- 4.

palindrome'' : (str : String) -> Bool
palindrome'' str = if length str > 10 
                      then checkPalindrome str
                      else False
  where
    checkPalindrome : String -> Bool
    checkPalindrome str = toLower str == reverse (toLower str)

-- 5.

palindrome''' : (n: Nat) -> (str : String) -> Bool
palindrome''' n str = if length str > n
                         then checkPalindrome str
                         else False
  where
    checkPalindrome : String -> Bool
    checkPalindrome str = toLower str == reverse (toLower str)

-- 6.

counts : (str : String) -> (Nat, Nat)
counts str = (wordCount, charCount)
  where
    wordCount : Nat
    wordCount = length (words str)

    charCount : Nat
    charCount = length str

-- 7.

topTen : Ord a => List a -> List a
topTen xs = take 10 $ sortBy (\x, y => compare y x) xs

-- 8

overLength : (n: Nat) -> (strs : List String) -> Nat
overLength n xs = length $ filter (\s => length s > n) xs

-- 9. See Palindrome.idr

-- 10. See Counts.idr