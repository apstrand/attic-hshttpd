
module OS_Proc where


import System
import IO

import SemiFFI

execve :: String -> [String] -> [String] -> IO Int
execve p a e = do
    let p' = toCString p
    a' <- toCLstString a
    e' <- toCLstString e
    c_exec p' a' e'

wait :: Int -> IO Int
wait pid = failif (-1) (c_wait pid)

runShellProc :: String -> [(String,String)] -> IO Int
runShellProc cmd env = do
    let env'  = map (\(k,v) -> k ++ "=" ++ v) env
    runProc "/bin/sh" ["/bin/sh","-c",cmd] env'

runProc :: String -> [String] -> [String] -> IO Int
runProc path args env = do
    r <- fork 1
    case r of
     Just pid -> do
	wait pid
     Nothing -> do
	r <- execve path args env
	exitWith (ExitFailure r)

fork :: Int -> IO (Maybe Int)
fork n = do
    p <- c_fork n
    case p of
     -1 -> error "fork error"
     0  -> return Nothing
     pid -> return (Just pid)






{- ffi "c_fork" c_fork :: Int -> IO Int -}
foreign import ccall "fork" c_fork :: Int -> IO Int
{- ffi "waitchild" c_wait :: Int -> IO Int -}
foreign import ccall "waitchild" c_wait :: Int -> IO Int
{- ffi "exec" c_exec :: CoutString -> Addr -> Addr -> IO Int -}
foreign import ccall "exec" c_exec :: CoutString -> Addr -> Addr -> IO Int




