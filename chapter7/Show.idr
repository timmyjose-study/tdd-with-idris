module Show 

%default total

record Album where
  constructor MkAlbum
  artist : String
  title : String
  year : Integer

{-
  interface Show ty where
    show : (x : ty) -> String
    showPrec : (d : Prec) -> (x : ty) -> String
-}

Show Album where
  show (MkAlbum artist title year) = 
    title ++ " by " ++ artist ++ " (released " ++ show year ++ ")"


help : Album
help = MkAlbum "The Beatles" "Help" 1965

rubbersoul : Album
rubbersoul = MkAlbum "The Beatles" "Rubber Soul" 1965

clouds : Album
clouds = MkAlbum "Joni Mitchell" "Clouds" 1969

hunkydory : Album
hunkydory = MkAlbum "David Bowie" "Hunky Dory" 1971

heroes : Album
heroes = MkAlbum "David Bowie" "Heroes" 1977

collection : List Album
collection = [help, rubbersoul, clouds, hunkydory, heroes]
