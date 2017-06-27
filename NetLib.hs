
module NetLib where

import IO
import Parser(Parser)
import Monad
import List

import MonadLib
import ParseExtra
import Uri
import Http
import IOFuncs
import OS_Types
import NetIO

data Req = Req_Gen String | Req_File String | Req_Http HttpUri | Req_Err String

data ServerData = Accepted RemoteData
		| SockReady WaitFor
    deriving Show

data RemoteData = RemoteData {
	rem_host    ::	HostAddress,
	rem_port    ::	Port,
	rem_time    ::	Time,
	rem_read    ::	Reader,
	rem_write   ::	Writer
    }
    deriving Show


server :: Int -> (ServerData -> a -> NetIO (Bool, a)) -> a -> IO ()
server p serv dt = do
	p <- listen_on (mkPort p)
	server' p [] dt []
    where

	server' p (r:rq) dt ws = do
	    ((b,dt'),ss) <- runNetIO (serv (SockReady r) dt)
                             `catch` (\_->close_handle (unWF r) >> return ((True,dt),[]))
	    when b (server' p rq dt' (ws++ss))

	server' p [] dt ws = do
	    (acc,rs,ws') <- waitforio (Wait_Read p) ws
	    if not acc
	     then server' p rs dt ws'
	     else do
	     	(s,hn,pr) <- accept_conn p
    		t <- time
		let sd = Accepted (RemoteData hn pr t (SR s) (SW s))
		((b,dt'),ss) <- runNetIO (serv sd dt)
                                 `catch` (\_->close_handle s >> return ((True, dt), []))
		when b (server' p rs dt' (ws'++ss))

netreader = do
    s <- getsock
    return (SR s)

httppost dt hdrs u = do
	sendreq Post u hdrs
	send (Str dt)
	netreader

httpget hdrs u = do
	sendreq Get u hdrs
	netreader

sendreq method uri hdrs = do
	debug 1 (show uri)
	connect uri
	send $ Str $ show method ++ " " ++ (path uri) ++ " HTTP/1.0"
	send (Strs (map show hdrs))
	send (Str "")
    
