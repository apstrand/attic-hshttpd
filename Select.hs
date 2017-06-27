
module Select (select) where

import Monad

import OS_Types

import SemiFFI

-- hbc.. grmbl...
filterM' :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM' p []      = return []
filterM' p (x:xs)  = do 
			b <- p x
			ys <- filterM' p xs
			return (if b then (x:ys) else ys)


select :: [WaitFor] -> IO [WaitFor]
select ws = do
    sl <- c_select_create
    mapM_ (select_add sl) ws
--    putStr "Waiting for IO on: " >> print ws
    r <- c_select_run sl
    res <- filterM' (select_test sl) ws
    c_select_destroy sl
--    putStr "IO ready on: " >> print res
    return res
 where
    select_add sl wf = do
	let (w,s) = decodewf wf
	r <- c_select_add sl w s
	when (r == -1) (fail "select_add error")
    select_test sl wf = do
	let (w,s) = decodewf wf
	t <- (c_select_test sl w s)
	return (if t == 0 then False else True)
    decodewf :: WaitFor -> (Int, Int)
    decodewf (Wait_Read (IOH s)) = (0,s)
    decodewf (Wait_Write (IOH s)) =  (1,s)
    decodewf (Wait_Exception (IOH s)) = (2,s)



{- ffi "select_create" c_select_create :: IO Addr -}
foreign import ccall "select_create" c_select_create :: IO Addr
{- ffi "select_destroy" c_select_destroy :: Addr -> IO () -}
foreign import ccall "select_destroy" c_select_destroy :: Addr -> IO ()
{- ffi "select_add" c_select_add :: Addr -> Int -> Int -> IO Int -}
foreign import ccall "select_add" c_select_add :: Addr -> Int -> Int -> IO Int
{- ffi "select_del" c_select_del :: Addr -> Int -> Int -> IO Int -}
foreign import ccall "select_del" c_select_del :: Addr -> Int -> Int -> IO Int
{- ffi "select_test" c_select_test :: Addr -> Int -> Int -> IO Int -}
foreign import ccall "select_test" c_select_test :: Addr -> Int -> Int -> IO Int
{- ffi "select_run" c_select_run :: Addr -> IO Int -}
foreign import ccall "select_run" c_select_run :: Addr -> IO Int

