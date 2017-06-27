
module OS_Net (
    HostAddress,
    Port,
    mkPort,
    IOHandle,
    connect_to,
    listen_on,
    accept_conn,
    inet_ntoa,
    addr2host,
    inAddr_Any
    ) where

import Monad

import SemiFFI
import IOExtra

import OS_Types

type HostName	    = String

newtype Port = Port Int
newtype HostAddress = HA Int


instance Show HostAddress where
    show addr = inet_ntoa addr

inAddr_Any  = HA 0

instance Show Port where
    show (Port p) = show p

mkPort :: Int -> Port
mkPort = Port

connect_to :: HostName -> Port -> IO IOHandle
connect_to s (Port p) =
    withCString s $ \hp -> 
    failif (-1) (c_connect_to hp p) >>= return.IOH

listen_on :: Port -> IO IOHandle
listen_on (Port p) = failif (-1) (c_listen_on p) >>= return.IOH

accept_conn :: IOHandle -> IO (IOHandle, HostAddress, Port)
accept_conn (IOH s) = do
    p <- c_accept_conn s
    when (p == nullAddr) ffail
    sk <- peek p 0
    ha <- peek p 4
    pr <- peek p 8
    return (IOH sk, HA ha, Port pr)

inet_ntoa :: HostAddress -> String
inet_ntoa (HA n) = unsafePerformIO (c_inet_ntoa n >>= fromCString)

addr2host :: HostAddress -> IO String
addr2host (HA n) = failif ("") (c_addr2host n >>= fromCString)

 
{- ffi "connect_to" c_connect_to :: CoutString -> Int -> IO Int -}
foreign import ccall "connect_to" c_connect_to :: CoutString -> Int -> IO Int
{- ffi "listen_on" c_listen_on :: Int -> IO Int -}
foreign import ccall "listen_on" c_listen_on :: Int -> IO Int
{- ffi "accept_conn" c_accept_conn :: Int -> IO Addr -}
foreign import ccall "accept_conn" c_accept_conn :: Int -> IO Addr
{- ffi "inet_ntoa" c_inet_ntoa :: Int -> IO CinString -}
foreign import ccall "inet_ntoa" c_inet_ntoa :: Int -> IO CinString
{- ffi "addr2host" c_addr2host :: Int -> IO CinString -}
foreign import ccall "addr2host" c_addr2host :: Int -> IO CinString



