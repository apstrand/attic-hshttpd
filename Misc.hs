
module Misc (stoi, cK, cS, retr, ifM, fst3, snd3, trd3, testsize) where



stoi :: String -> Int
stoi s = case reads s of
	    ((p,_):_) -> p
	    _ -> 0

retr :: Eq a => a -> [(a,b)] -> (Maybe b, [(a,b)])
retr key list = retr' key list []
 where
  retr' _ [] vs = (Nothing, vs)
  retr' key ((a,b):xs) vs
   | key == a  = (Just b, vs++xs)
   | otherwise = retr' key xs ((a,b):vs)

cK = \x y -> x
cS = \x y z -> (x z) (y z) -- ???
cI = cS cK cK

ifM m f = maybe (return ()) f m


fst3 (a,_,_) = a
snd3 (_,b,_) = b
trd3 (_,_,c) = c

testsize [] _ e = e
testsize xs f _ = f xs

