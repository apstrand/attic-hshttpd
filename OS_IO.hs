
module OS_IO (
    open_file,
    close_handle,
    write_n,
    read_char,
    write_char,
    transfer_n,
    transfer_all,
    file_size,
    file_type,
    WaitFor(..),
    OpenMode(..),
    FileType(..)
    ) where

import Monad
import Char
import IOExtra

import OS_Types
import SemiFFI

write_n :: IOHandle -> (Int,String) -> IO ()
write_n (IOH s) (i,d) = 
    withCString d (\p -> failif (-1) (c_write_n s i p) >> return ())


read_char :: IOHandle -> IO Char
read_char = liftM chr . failif (-1) . c_read_char . unIOH


write_char :: IOHandle -> Char -> IO ()
write_char (IOH s) c = 
    failif (-1) (c_write_char s c) >> return ()


transfer_n :: IOHandle -> IOHandle -> Int -> IO ()
transfer_n (IOH s) (IOH d) n = 
    failif (-1) (c_transfer_n s d n) >> return ()

transfer_all :: IOHandle -> IOHandle -> IO ()
transfer_all s d = transfer_n s d 0

open_file :: String -> OpenMode -> IO IOHandle
open_file f m = withCString f $ \p -> 
    failif (-1) (c_open_file p (fromEnum m)) >>= return.IOH

file_size :: String -> IO Int
file_size f = withCString f $ \p -> failif (-1) (c_file_size p)

file_type :: String -> IO FileType
file_type f = withCString f $ \p -> failif (-1) (c_file_type p) >>= return.toEnum

close_handle :: IOHandle -> IO ()
close_handle (IOH s) = c_close_handle s >> return ()





{- ffi "read_char" c_read_char :: Int -> IO Int -}
foreign import ccall "read_char" c_read_char :: Int -> IO Int
{- ffi "write_char" c_write_char :: Int -> Char -> IO Int -}
foreign import ccall "write_char" c_write_char :: Int -> Char -> IO Int
{- ffi "write_n" c_write_n :: Int -> Int -> CoutString -> IO Int -}
foreign import ccall "write_n" c_write_n :: Int -> Int -> CoutString -> IO Int
{- ffi "transfer_n" c_transfer_n :: Int -> Int -> Int -> IO Int -}
foreign import ccall "transfer_n" c_transfer_n :: Int -> Int -> Int -> IO Int
{- ffi "open_file" c_open_file :: CoutString -> Int -> IO Int -}
foreign import ccall "open_file" c_open_file :: CoutString -> Int -> IO Int
{- ffi "close" c_close_handle :: Int -> IO Int -}
foreign import ccall "close" c_close_handle :: Int -> IO Int
{- ffi "file_size" c_file_size :: CoutString -> IO Int -}
foreign import ccall "file_size" c_file_size :: CoutString -> IO Int
{- ffi "file_type" c_file_type :: CoutString -> IO Int -}
foreign import ccall "file_type" c_file_type :: CoutString -> IO Int



