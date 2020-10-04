module Maybe

%default total

maybeAdd : Maybe Int -> Maybe Int -> Maybe Int
maybeAdd x y = case x of
                    Nothing => Nothing
                    Just a => case y of
                                   Nothing => Nothing
                                   Just b => Just (a + b)

mAdd : Maybe Int -> Maybe Int -> Maybe Int
mAdd mx my = do x <- mx
                y <- my
                Just (x + y)