
module IOFuncs(
    IOHandle,
    WaitFor(..),
    Reader(..),
    Writer(..),
    stdErrWriter,
    readchar,  readline,  readlines, read_n, readsocket,
    writechar, writeline, writelines, writesocket, write_str, write_n,
    transfer, toW, toR,
    CanClose(..),
    ioeGetError,
    PosixError(..),
    intToPosixError,
    module OS_Misc,
    module OS_Net,
    module OS_IO
    ) where

import OS_Misc
import OS_Net
import OS_IO
import OS_Types
import IOExtra

import Directory
import IO
import Monad

import Mime
import HttpStatus

data Reader = SR IOHandle | HR Handle

toR = HR

instance Show Reader where
    show (SR s) = "<socket reader: " ++ show s ++ ">"
    show (HR _) = "<handle reader>"

readchar (SR s) = read_char s
readchar (HR h) = hGetChar h

readline s = readline' [] >>= return.reverse
    where
	readline' xs = do
	    c <- (readchar s) `catch` (\_ -> return '\n')
	    case c of
	     '\r'   -> readline' xs
	     '\n'   -> return xs
	     _      -> readline' (c:xs)

readlines r = do
    l <- readline r
    if l == [] then return [l] else do ls <- readlines r ; return (l:ls)

read_n s n = mapM (\_ -> readchar s) [1..n]

readsocket (SR s) = Just s
readsocket _ = Nothing

data Writer = SW IOHandle | HW Handle

toW = HW

instance Show Writer where
    show (SW s) = "<socket writer: " ++ show s ++ ">"
    show (HW _) = "<handle writer>"

stdErrWriter = HW stderr

writechar (SW s) = write_char s
writechar (HW c) = hPutChar c
writeline s ln = mapM_ (writechar s) ln >> writechar s '\r' >> writechar s '\n'

writelines s xs = case xs of
    [] -> return ()
    (x:xs) -> writeline s x >> writelines s xs
writesocket (SW s) = Just s

write_str (SW s) str = write_n s (length str, str)

transfer read write =
    case (readsocket read, writesocket write) of
	(Just r, Just w) -> (transfer_all r w >> close_handle r)
                             `catch` (\e -> close_handle r >> close_handle w >> ioError e)
	_		 -> error "no transfer match..."

class CanClose a where
    close :: a -> IO ()
instance CanClose Reader where
    close (SR s) = close_handle s
    close (HR h) = hClose h
instance CanClose Writer where
    close (SW s) = close_handle s
    close (HW h) = hClose h


