
module OS_Misc (
    Time,
    time,
    strftime
    ) where

import IOExtra
import SemiFFI


type Time = Int

time :: IO Time
time = c_time 0

strftime :: String -> Time -> String
strftime fmt t = unsafePerformIO $ do
    withCString fmt $ \p -> 
        c_strftime p t >>= fromCString







{- ffi "time" c_time :: Int -> IO Int -}
foreign import ccall "time" c_time :: Int -> IO Int
{- ffi "strftime_" c_strftime :: CoutString -> Int -> IO CinString -}
foreign import ccall "strftime_" c_strftime :: CoutString -> Int -> IO CinString





