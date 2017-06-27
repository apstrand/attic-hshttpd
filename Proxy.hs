
module Proxy(handleProxy, replaceBanner) where

import IO
import List

import NetIO
import NetLib
import Http
import HttpProto
import Mime
import Uri
import HttpStatus
import IOLib
import CombUtils
import ParseExtra(doparse)
 
http_retr GET _ = httpget
http_retr HEAD _ = httpget
http_retr POST d = httppost d

urllist = ["http://ads.freshmeat.net", "http://ad.doubleclick.net"]


replaceBanner = simpleHandler $ \req -> do
    let path = http_path req
    case foldr (||) False (map (flip isPrefixOf path) urllist) of
     False  ->	return req
     True   ->  return (req { http_path = "/images/1x1.png" })


handleProxy = simpleHandler $ \req -> do
    let path = http_path req 
    case doparse parse_uri path of
	 Nothing -> return (req { http_status = (BadRequest,"Invalid URL: " ++ path) })
	 Just u  -> do
		     r <- runNetIO (http_retr (http_req req) (http_reqdata req) (http_reqhdrs req) u)
		     return (req { http_status = (OK,""), 
				   http_respdata = Just (Contents unknown 0 (Right r)) })
				
    

