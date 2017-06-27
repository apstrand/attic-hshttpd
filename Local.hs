
module Local where

import Http
import HttpStatus
import IOFuncs
import Conf

finit status hdrs contents req = 
    Next (req { http_status = status, 
		http_resphdrs = hdrs, 
		http_respdata = contents
	      })

respFile s m h = finit (R_OK, "OK") [] (Just (Contents m s (Right (h))))
fileErr path err = finit (parseError err) [] Nothing

updDocroot old (Docroot new) = new
updDocroot old _ = old


parseError :: IOError -> (StatusCode, String)
parseError err = let errstr = ioeGetError err in 
	case reads errstr of
	[(c,r)]	->  (case intToPosixError c of
		      EPERM	->  R_Forbidden
		      ENOENT	->  R_NotFound
		      EACCES	->  R_Forbidden
		      _		->  R_IntErr
		     , tail r)
	_	->  (R_IntErr, errstr)

