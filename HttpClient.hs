
module HttpClient where

import Maybe

import HttpProto
import HttpStatus
import Uri
import Misc
import Mime
import IOLib
import ParseExtra
import HttpProto
import OS_Types

data Request = Http_Get Uri [Header]
	     | Http_Post Uri [Header] [(String,String)]
	     | Http_PostRaw Uri [Header] String
	     | Http_Raw String Int String
    deriving Show

data Response = Success [Header] String
	      | Fail StatusCode String
    deriving Show

http_simple_get :: String -> IO ([String], String)
http_simple_get s = case read_uri s of
    uri@(Http_Uri a h po p q) -> do
	let req = unlines (mkreq "GET" h p q)
	resp <- http_get (Http_Raw h po req)
	case resp of
	 Success hdrs cnt -> return (map show hdrs, cnt)
	 Fail c s -> fail (show c ++ ": " ++ s)
    _ -> fail "Uri parse error!"

http_get :: Request -> IO Response

http_get (Http_Raw host port cnts) = do
    s' <- connect_to host (mkPort port)
    let s = SR s'
    write_str (SW s') cnts
    hdrs <- readlines s
    mapM putStrLn hdrs
    cnts <- read_str s
    return (Success (catMaybes (map (doparse httpheaders) hdrs)) cnts)

http_get (Http_Get (Http_Uri a h po p q) hdrs) = do
    let req = unlines (mkreq "GET" h p q ++ map show hdrs)
    http_get (Http_Raw h po req)

http_get (Http_Post uri hdrs pd) =
    http_get (Http_PostRaw uri hdrs (unlines (map (\(var,val) -> var ++ "=" ++ val) pd)))

http_get (Http_PostRaw (Http_Uri a h po p q) hdrs pd) = do
    let req = unlines (mkreq "POST" h p q ++ map show hdrs
		      ++ map show [Content_Type mimeform, Content_Length (length pd)] ++ [[]])
	     ++ pd
    http_get (Http_Raw h po req)
    
mkreq t h p q = (t ++ " " ++ p ++ (testsize q ('?':) "") ++ " HTTP/1.0"):
		(map show [Host h, UserAgent "hsget/0.1"]) ++ [[]]

