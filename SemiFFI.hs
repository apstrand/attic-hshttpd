module SemiFFI (
    failif,
    ffail,
    fromCString,
    withCString,
    toCLstString,
    CinString,
    CoutString,
    CLstoutString,
    Addr,
    nullAddr,
    plusAddr,

    peek,poke,

    get_errmsg, get_errcode
    ) where

import Char
import Monad



import qualified Addr as A
import qualified Foreign as FFI
import CString

type CinString = CString
type CoutString = CString

toCString = newCString

fromCString = peekCString

type Addr = FFI.Ptr ()

peek p o = FFI.peekByteOff p (fromIntegral o)
poke p o v = FFI.pokeByteOff p (fromIntegral o) v
nullAddr = FFI.nullPtr
plusAddr = FFI.plusPtr





foreign import ccall "get_errmsg" get_errmsg :: IO CinString
foreign import ccall "get_errcode" get_errcode :: IO Int
foreign import ccall "malloc" malloc :: Int -> IO Addr
foreign import ccall "free" free :: Addr -> IO ()


type CLstoutString = FFI.Ptr Char
toCLstString :: [String] -> IO CLstoutString
toCLstString ls = do
    let len = length ls
    p <- FFI.castPtr `liftM` malloc (len * 4 + 4)
    cs <- mapM toCString ls
    mapM_ (\(b,o) -> poke p o b) (zip cs [0,4..])
    poke p (len*4) (0::Int)
    return p



get_error :: IO (Int, String)
get_error = do
    p <- get_errmsg >>= peekCString . FFI.castPtr
    i <- get_errcode
    return (i, p)


ffail = get_error >>= ioError.userError.snd

failif v m = do
    r <- m
    if r == v then ffail else return r


