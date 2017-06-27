
module Logging(logwriter) where

import IOFuncs
import Http
import HttpProto
import HttpStatus
import Conf
import CombUtils
import Misc

logformat [] _ = []
logformat ('%':x:xs) dt@req =
	( case x of
			'p' -> http_path req
			'r' -> show (http_req req)
			'a' -> inet_ntoa (fst3 (http_remote req))
			'h' -> show (http_reqhdrs req)
			's' -> show (http_status req)
			'd' -> strftime "%a %b %d %T %Z %Y" (trd3 (http_remote req))
			_   -> [x]
	):logformat xs dt
logformat (x:xs) dt = [x]:logformat xs dt

updLog (old,w) (Logformat new) = (new,w)
updLog (old,_) (Logwriter w) = (old,Just w)
updLog old _ = old

logwriter = confHandler ("%r %p [%d] %a %h %s", Nothing) updLog writelog

writelog (fmt, wr) req = return (Next (req { http_finish = logwriter }))
    where logwriter req' = do
		maybe (return ()) (\wr -> writeline wr (concat (logformat fmt req'))) wr
		(http_finish req) req'


