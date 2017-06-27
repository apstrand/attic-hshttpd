
module Http 
     where

import Monad
import System
import Char

import IOFuncs
import HttpStatus
import ParseExtra
import Parser
import Mime
import HttpProto

import Conf
import Uri

data ReqType = Get | Post | Head | Err
    deriving Eq

data HttpUri = HttpUri (Maybe (String,String)) String Int String String

instance Show HttpUri where
	show (HttpUri a h p pa q) = "http://" ++ h



instance Show ReqType where
    show Get   = "GET"
    show Post  = "POST"
    show Head  = "HEAD"
    show Err   = "ERR"

type WaitQ = [(WaitFor, (WaitFor -> IO Bool))]

type Finisher = ReqData -> IO (Either ReqData WaitQ)

data ReqData = ReqData {
	http_req	::  ReqType,
	http_path	::  String,
	http_ver	::  (Int,Int),
	http_sendhdrs	::  Bool,
	http_reqhdrs	::  [Header],
	http_reqdata	::  String,
	http_remote	::  (HostAddress, Port, Time),
	http_status	::  (StatusCode, String),
	http_resphdrs	::  [Header],
	http_respdata	::  Maybe Contents,
	http_finish	::  Finisher
	}

data Contents = Contents {
	contents_mime	::  MimeType,
	contents_length	::  Int,
	contents_read	::  Either String Reader
    }

data Event = Configure Conf
	   | Report [String] ReqData ([String] -> Finisher)
	   | Restart Event
	   | Next ReqData
	   | Ready WaitFor
	   | Wait WaitFor
	   | Null

newtype Handler = Handler HandlerT
runHandler (Handler h) = h

type HandlerT = Event -> IO (Event, Handler)


httpver :: Parser (Int, Int)
httpver = do
	istring "http/"
	major <- nat
	lit '.'
	minor <- nat
	return (major, minor)

httpreq :: Parser (ReqType, String, String, (Int,Int))
httpreq = do
	s <- many letter
	lit ' '
	p <- px_path
	q <- many0 (sat (not.isSpace))
	v <- (lit ' ' >> httpver) +++ return (0,9)
	let t = case s of
	     "GET"	->  Get
	     "HEAD"	->  Head
	     "POST"	->  Post
	     _	->  Err
	return (t,p,q,v)

