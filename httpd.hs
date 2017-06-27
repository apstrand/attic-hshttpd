
module Main where

import IO
import List
import System
import Monad
import Maybe

import Http
import HttpProto
import HttpStatus
import IOFuncs
import NetIO
import NetLib
import MonadLib

import MainLoop
import LocalFile
import Rewrite
import Logging
import Conf
import Http
import CombUtils
import Misc



reqLocal = reqMatcher ["^/"] "local"

reqNotHandled =
    simpleMaybeHandler "default" (\req -> (fst (http_status req)) == R_NotHandled)

quitHandler = Handler $ \ev -> case ev of
    Next req -> print ("quit requested!") >> exitWith ExitSuccess
    _ -> return (ev,quitHandler)

showit = Handler $ \ev -> case ev of
    Next req	    -> putStrLn "req " >> print (http_status req) >> return (ev, showit)
    Report xs _ _   -> putStrLn ("report " ++ show xs) >> return (ev,showit)
    Restart ev	    -> putStrLn "restart" >> return (ev,showit)
    _		    -> print "other" >> return (ev, showit)

type AcceptQ = [(WaitFor,RemoteData)]

handleconn :: ServerData -> ((Handler,WaitQ),AcceptQ) -> NetIO (Bool, ((Handler,WaitQ),AcceptQ))

handleconn (Accepted rd) ((hd,wq),aq) = do
		let sock = fromJust (readsocket (rem_read rd))
		pushWait (Wait_Read sock)
		return (True, ((hd,wq), (Wait_Read sock, rd):aq))


handleconn (SockReady wf) ((hd,wq),aq) = do
    let (isacc,aq') = retr wf aq
    ev' <- case isacc of
	    Just rd -> liftM Next $ io $ readHttpReq rd
	    Nothing -> return (Ready wf)
    (ws,(hd',wq')) <- io $ httpDispatch (hd,wq) ev'
    mapM_ pushWait ws
    return (True, ((hd', wq'), aq'))

handlers = (
	rewriter ->- 
	reqLocal (
	    checkurl (
		    handleDir ->- 
		    reqMatcher ["\\.html$"] "html" fileHandler ->-
		    reqMatcher ["\\.txt$"] "text" fileHandler ->-
                    reqMatcher ["/quit"] "quit" quitHandler ->-
		    reqNotHandled fileHandler)) ->-
	logwriter
    )


main = do
    a <- getArgs
    conf <- parseargs a
    let htdisp h ev = httpDispatch (h,[]) ev >>= return.fst.snd
    hd <- foldM htdisp handlers (map Configure conf)
    server (maybe 8080 id (getPort conf)) handleconn ((hd,[]),[])


