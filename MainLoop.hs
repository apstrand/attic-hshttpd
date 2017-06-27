
module MainLoop where

import NetLib
import IOFuncs
import HttpProto
import HttpStatus
import Http
import Misc
import Conf
import Parser
import ParseExtra

emptyrequest :: ReqData
emptyrequest = ReqData Err "" (0,0) False [] "" (inAddr_Any,mkPort 0,0)
		(R_NotHandled,"") [] Nothing (\_->print "empty" >> return (Right []))


readHttpReq :: RemoteData -> IO ReqData
readHttpReq rd = do
    s <- readline (rem_read rd)
    case doparse httpreq s of
     Nothing ->	badreq rd
     Just (r,p,q,v)  ->
       do
	hdr <- if fst v > 0 then readheaders rd else return []
	ptdt <- case r of
	 Post   ->	do
			    let [len] = [len | Content_Length len <- hdr]
			    cnt <- read_n (rem_read rd) len
			    return cnt
	 _	     ->	return q
	return (emptyrequest {	http_req = r,
				http_path = p,
				http_ver = v,
				http_sendhdrs = v == (0,9),
				http_reqhdrs = hdr,
				http_reqdata = ptdt,
				http_remote = (rem_host rd, rem_port rd, rem_time rd),
				http_finish = end_finisher rd
			    } )
   where
	badreq rd = return $ emptyrequest { http_status = (R_BadReq, show R_BadReq) }
	readheaders rd = do
	    ls <- readlines (rem_read rd)
	    return [m | Just m <- map (doparse httpheaders) ls]
	end_finisher rd req = sendresponse rd req >>= return.Right

getlen (Content_Length len) = Just len
getlen _ = Nothing

httpDispatch :: (Handler, WaitQ) -> Event -> IO ([WaitFor], (Handler, WaitQ))
httpDispatch (h,wq) (Ready a) = case lookup a wq of
    Just f -> transfer_it a f >>= \b ->  return (if b then [a] else [],(h,wq))
    Nothing -> httpDispatch' h wq (Ready a)
httpDispatch (h,wq) ev = httpDispatch' h wq ev

httpDispatch' :: Handler -> WaitQ -> Event -> IO ([WaitFor], (Handler, WaitQ))
httpDispatch' h wq ev = do
    (ev', h') <- runHandler h ev
    case ev' of
     Wait ws -> return ([ws],(h',wq))
     Restart ev     -> httpDispatch (h', wq) ev
     Next req	    -> do
	res <- (http_finish req) req
	case res of
	 Left req -> httpDispatch (h', wq) (Next req)
	 Right wq' -> return ((map fst wq'),(h',wq++wq'))
     Ready a -> return ([],(h',wq))
     Report lst req fin -> do 
	res <- fin lst req
	case res of
	 Left req -> httpDispatch (h', wq) (Next req)
	 Right wq' -> return ((map fst wq'),(h',wq++wq'))
	 
     _ -> return ([],(h',wq))

transfer_it h f = do
    f h
    return False
 `catch` (\e -> putStrLn ("exception: " ++ show e) >> return (False))

sendresponse rd req = do
    when (fst (http_status req) /= R_UnParsed)
	    (sendheaders (http_sendhdrs req) rd req)
    sendcontents rd req

sendheaders :: Bool -> RemoteData -> ReqData -> IO ()
sendheaders sh rd req = 
    writelines (rem_write rd) (if sh then [head shdrs] else shdrs)
     where  shdrs = map show (makeheaders req) ++ [""]
	    makeheaders req = 
		[Status (code2num code) (show code),
		 Server "hshttpd/0.02"] ++ (getheaders req)
	    code = (fst.http_status) req
	    getheaders req = http_resphdrs req ++ 
		(maybe [] (\(Contents mime len _) -> 
			    [Content_Length len, Content_Type mime]) (http_respdata req))

sendcontents :: RemoteData -> ReqData -> IO WaitQ
sendcontents rd req = do
    let (sc,ss) = http_status req
	cnts    = http_respdata req
    case (elem (http_req req) [Get,Post], cnts, sc) of
     (True,Just cn,R_OK)    -> case contents_read cn of
				Right s -> case readsocket s of
					    Just s -> return [(Wait_Read s, \(Wait_Read s) -> transfer (SR s) (rem_write rd) >> close (rem_write rd) >> return False)]
	    				    Nothing -> transfer s (rem_write rd) >> return []
				Left str -> write_str (rem_write rd) str >> close (rem_write rd) >> return []
     (True, _, _)	    -> writeline (rem_write rd) (errorpage req) >> close (rem_write rd) >> return []
     _			    -> return []


errorpage res = "\r\n<html><head><title>" ++  ((show.code2num) status) ++ " - "
	     ++ (show status) ++ "</title></head><body><h1>"
	     ++ (show status) ++ "</h1><br><h2>"
	     ++ (htmlescape msg) ++ "</body></html>"
    where (status,msg) = http_status res


