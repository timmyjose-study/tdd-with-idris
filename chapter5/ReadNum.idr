module ReadNum

import Data.Strings

%default total

readNumber : HasIO io => io (Maybe Nat)
readNumber = do n <- getLine
                if all isDigit (unpack n) 
                   then pure $ Just (stringToNatOrZ n)
                   else pure Nothing

readNumbers : HasIO io => io (Maybe (Nat, Nat))
readNumbers = do m <- readNumber
                 case m of
                      Nothing => pure Nothing
                      Just mm => do n <- readNumber
                                    case n of
                                         Nothing => pure Nothing
                                         Just nn => pure $ Just (mm, nn)

-- pattern-matched destucturing

readNumbersImproved : HasIO io => io (Maybe (Nat, Nat))
readNumbersImproved = do Just m <- readNumber | Nothing => pure Nothing 
                         Just n <- readNumber | Nothing => pure Nothing
                         pure $ Just (m, n)