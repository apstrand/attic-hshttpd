
module RX (
    RX,
    compile,
    scompile,
    match,
    submatch,
    matchReplace,
    replace
    )
    where

import Char
import Maybe

import SemiFFI
import IOExtra

newtype RX = RX Addr

compile :: String -> Maybe RX
compile s = let p = unsafePerformIO (withCString s c_rx_compile)
    in if p == nullAddr then Nothing else Just (RX p)

scompile :: String -> RX
scompile = maybe failmatch id . compile

failmatch = fromJust (compile "^$")

match :: RX -> String -> Bool
match (RX p) s = case (unsafePerformIO (withCString s (c_rx_match p))) of
    0 -> False
    _ -> True

submatch :: RX -> String -> [String]
submatch (RX p) s = let n = unsafePerformIO (withCString s (c_rx_submatch p))
    in if n == 0 then [] else unsafePerformIO (mapM getmatch [1..n])

getmatch :: Int -> IO String
getmatch n = c_rx_getmatch n >>= fromCString

matchReplace :: RX -> String -> String -> String
matchReplace rx rp str = replace rp (submatch rx str)

replace "" rs = ""
replace (x:[]) rs = [x]
replace (e:d:xs) rs
    | e == '\\' && isDigit d = (index rs d) ++ (replace xs rs)
    | otherwise = e:(replace (d:xs) rs)
 where index [] _ = []
       index (r:_) '1' = r
       index (_:rs) c = index rs (pred c)



{- ffi "rx_compile" c_rx_compile :: CoutString -> IO Addr -}
foreign import ccall "rx_compile" c_rx_compile :: CoutString -> IO Addr
{- ffi "rx_match" c_rx_match :: Addr -> CoutString -> IO Int -}
foreign import ccall "rx_match" c_rx_match :: Addr -> CoutString -> IO Int
{- ffi "rx_submatch" c_rx_submatch :: Addr -> CoutString -> IO Int -}
foreign import ccall "rx_submatch" c_rx_submatch :: Addr -> CoutString -> IO Int
{- ffi "rx_getmatch" c_rx_getmatch :: Int -> IO CinString -}
foreign import ccall "rx_getmatch" c_rx_getmatch :: Int -> IO CinString


