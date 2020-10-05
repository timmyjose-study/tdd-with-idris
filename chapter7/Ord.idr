module Ord 

import Data.List

%default total

{-
  interface Eq ty => Ord ty where
    compare : ty -> ty -> Ordering -- required
    (<) : ty -> ty -> Ordering
    (>) : ty -> ty -> Ordering
    (<=) : ty -> ty -> Ordering
    (>=) : ty -> ty -> Ordering
    max : ty -> ty -> ty
    min : ty -> ty -> ty
-}

record Album where
  constructor MkAlbum
  artist : String
  title : String
  year : Integer

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

Eq Album where
  (==) (MkAlbum artist title year) (MkAlbum artist' title' year') =
    artist == artist' && title == title' && year == year'
  (==) _ _  = False

Ord Album where
  compare (MkAlbum artist title year) (MkAlbum artist' title' year') =
    case compare artist artist' of
         EQ => case compare year year' of
                    EQ => compare title title'
                    diffYear => diffYear
         diffArtist => diffArtist
