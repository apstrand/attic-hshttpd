
module NetIO where

import IO
import List

import Parser(Parser)
import IOFuncs
import MonadLib
import Uri
import OS_Types
import Select

data NetState = NetState {
	net_socket  :: Maybe IOHandle,
	net_debug   :: Int,
	net_listen  :: Maybe IOHandle,
	net_conn    :: Bool,
	net_log	    :: Writer,
	net_select  :: [WaitFor]
	}

data DataType = Str String | Strs [String]

type NetIO a = IOState NetState a

initnetstate = NetState Nothing 5 Nothing False stdErrWriter []

runNetIO :: NetIO a -> (IO (a, [WaitFor]))
runNetIO s = do
	let i = initnetstate
	r <- (runstp s initnetstate)
	return (fst r, net_select (snd r))

runSubNetIO :: NetIO a -> NetState -> NetIO a
runSubNetIO s st = do
	r <- io $ runstp s st
	return (fst r)

{-
runSub :: NetIO a b -> a -> NetIO c (b,a)
runSub m b = do
	ns@(NetState _ a _ _ _) <- getst
	(c,b') <- io $ runstp m (ns { net_srv = b })
	return (c, net_srv b')
-}	

getsock :: NetIO IOHandle
getsock = do
	    s <- getstp net_socket
    	    case s of
	     Nothing	->  fail "<internal error: socket list empty>"
	     Just h	->  return h

isConn :: NetIO Bool
isConn = getstp net_conn

connect u = do
	sock <- io $ connect_to (host u) (mkPort (port u))
	modst (\s -> s { net_socket = Just sock})
	return ()

netclose :: NetIO ()
netclose = do
	s <- getsock
	io $ close_handle s
	return ()


send (Str s) = do
	h <- getsock
	io $ write_n h ((length s) + 2,s++"\r\n") 

send (Strs xs) = case xs of
    []	-> return ()
    (x:xs) -> send (Str x) >> send (Strs xs)

debug l s = do
	a <- getstp net_debug
	if l <= a then io (putStrLn (show s)) else return ()


listen p = do
	s <- io $ listen_on p
	debug 1 ("Listening on: " ++ show s)
	modst (\st -> st { net_listen = Just s } )
	return s

accept s = do
	case s of
	 Just s -> do
	    (h,hn,p) <- accept_conn s
	    return (h,p,hn)
	 Nothing -> error "<not listening>"

waitforio :: WaitFor -> [WaitFor] -> IO (Bool, [WaitFor], [WaitFor])
waitforio ls ws = do
--    (ws,ls) <- getstp (\st -> (net_select st, maybe [] ((:[]) . Wait_Read) (net_listen st)))
    rs <- select (ls : ws)
    return (ls `elem` rs, rs, ws \\ rs)

{-
    modst (\st -> st { net_select = ws \\ rs } )
    if ls == []
     then return (False, rs)
     else return ((head ls) `elem` rs, rs \\ ls)
-}

pushWait :: WaitFor -> NetIO ()
pushWait wf = modst (\st -> st {net_select = wf:(net_select st)})

