
module Mux where

import List

import Http
import HttpStatus
import OS_IO
import IOLib
import CombUtils

type Suspend = (WaitFor, ReqData)

type MuxEv = Either Suspend Event

type MuxHandler st = st -> MuxEv -> IO (MuxEv, st)

muxHandler :: MuxHandler st -> st -> Handler
muxHandler mh st = Handler $ muxHandler' [] mh st

muxHandler' :: [Suspend] -> MuxHandler st -> st -> HandlerT
muxHandler' hs mh st ev@(Ready ws) = do
    case lookup ws hs of
     Nothing -> return (ev, Handler $ muxHandler' hs mh st)
     Just req -> do
		    (ev',st') <- mh st (Left (ws,req))
		    muxHfin ev' hs mh st'

muxHandler' hs mh st ev@(Next req) = do
    (ev',st') <- mh st (Right ev)
    muxHfin ev' hs mh st'
 `catch` (\err -> print err 
	       >> return (Next (req { http_status = (OK, "err mux")}), muxHandler mh st))

muxHandler' hs mh st ev = return (ev, Handler $ muxHandler' hs mh st)

muxHfin ev hs mh st = return (ev', Handler $ muxHandler' hs' mh st)
 where (ev',hs') = either (\s@(h,r) -> (Wait h, s:hs)) (\(Next e)->(Next e,hs)) ev



muxh :: () -> MuxEv -> IO (MuxEv, ())
muxh () (Right (Next req)) = do
    print "before open"
    h <- open_file "pipe" Reading
    print "after open"
    return (Left (Wait_Read h, req), ())
muxh () (Left (Wait_Read h, req)) = do
    print "before read"
    l <- readline h
    print "after read"
    close h
    print l
    return (Right (Next req { http_status = (OK, "ugh") } ), ())

