module Chapter3

%default total

describeList : List Int -> String
describeList [] = "Empty"
describeList (_ :: xs) = "Non-Empty with tail " ++ show xs

mutual
  isEven : Nat -> Bool
  isEven Z = True
  isEven (S k) = isOdd k

  isOdd : Nat -> Bool
  isOdd Z = False
  isOdd (S k) = isEven k