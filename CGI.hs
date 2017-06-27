
module CGI where

import Directory(removeFile)
import System

import Local

import ParseExtra(doparse)
import Parser(string)
import CombUtils
import Http
import HttpHeader
import HttpProto
import HttpStatus
import IOLib
import Mime
import Misc
import OS_Proc

cgiHandler = confHandler "./" updDocroot handleCGI'

req2env :: ReqData -> [(String,String)]
req2env (ReqData typ path ver _ hdrs post (ha,p,t) _ _ _ _) =
    [("QUERY_STRING",	snd (break (=='?') path)),
     ("REMOTE_ADDR",	show ha),
     ("SCRIPT_NAME",	(fst (break (=='?') path))),
     ("HTTP_USER_AGENT", maybeHead "" [u | UserAgent u <- hdrs]),
     ("HTTP_ACCEPT_ENCODING", maybeHead "" [e | AcceptEncoding e <- hdrs])
    ]

handleCGI' :: String -> ReqData -> IO Event
handleCGI' root req = do
    let path = http_path req
	abspath = root ++ path
    [tmpsin, tmpsout, tmpserr] <- mapM (cK tempname) [1..3]
    writeFile tmpsin (http_reqdata req)
    let cmd = abspath ++ " <" ++ tmpsin ++ " >" ++ tmpsout ++ " 2>" ++ tmpserr
	env = req2env req
    r <- runShellProc cmd env
    removeFile tmpsin
    case r of
     0 -> do
	    	size <- file_size tmpsout
		h <- open_file tmpsout Reading
		mapM_ removeFile [tmpsout,tmpserr]
		hdrs <- readlines h
		let hdr = head hdrs
		    mime = maybe unknown id (doparse (string "Content-type: " >> mimep) hdr)
		return (respFile (fromIntegral size) mime h req)
     _ -> do	err <- readFile tmpserr
		removeFile tmpserr
		return (finit (InternalServerError, show r ++ ": " ++ err) [] Nothing req)
  `catch`
	 (\err -> return (fileErr (http_path req) err req))

