module HttpProto  where


import Char

import Mime
import ParseExtra
import Parser
import HttpStatus
import Uri(Uri(..))

httpheaders :: Parser Header
httpheaders =
	    schead "host"	    (many (letter +++ digit +++ lit '.')) Host
	+++ schead "encoding"	    (sepby mimep csdelim) Encoding
	+++ schead "server"		line Server
	+++ schead "date"		line Date
	
	+++ schead "authorization"	line Authorization
	+++ schead "from"		line From
	+++ schead "if-modified-since"	line IfModSince
	+++ schead "referer"		line Referer
	+++ schead "user-agent"		line UserAgent
	
	+++ schead "allow"		line Allow
	+++ schead "content-encoding"	line Content_Encoding
	+++ schead "content-length"	nat Content_Length
	+++ schead "content-type"	mimep Content_Type
	+++ schead "expires"		line Expires
	+++ schead "last-modified"	line Last_Modified

htmlescape "" = ""
htmlescape ('\n':xs) = "<br>" ++ (htmlescape xs)
htmlescape (x:xs) = x:(htmlescape xs)


data Header = Status Int String
		 | Date String
		 | Server String
		 | Host String
		 | Encoding [MimeType]
		 | Unknown String

	-- Request header fields
		 | Authorization String
		 | From		 String
		 | IfModSince	 String
		 | Referer	 String
		 | UserAgent	 String
	
	-- Entity header fields
		 | Allow	    String
		 | Content_Encoding String
		 | Content_Length   Int
		 | Content_Type	    MimeType
		 | Expires	    String
		 | Last_Modified    String


		 

instance Show Header where
	show (Status d r)	= "HTTP/1.0 " ++ show d ++ " " ++ r
	show (Server s)		= "Server: " ++ s
	show (Date d)		= "Date: " ++ d
	show (Host h)		= "Host: " ++ h
	show (Encoding e)	= "Encoding: " ++ showcsdelim e
	show (Unknown u)	= u
	
	show (Authorization s)	= "Authorization: "	++ s
	show (From s)		= "From: "		++ s
	show (IfModSince s)	= "If-Modified-Since: " ++ s
	show (Referer s)	= "Referer: "		++ s
	show (UserAgent u)	= "User-Agent: "	++ u
	
	show (Allow s)		= "Allow: "		++ s
	show (Content_Encoding e) = "Content-Encoding: " ++ e
	show (Content_Length l) = "Content-Length: "	++ show l
	show (Content_Type t)	= "Content-Type: "	++ show t
	show (Expires s)	= "Expires: "		++ show s
	show (Last_Modified s)	= "Last-Modified: "	++ show s

unknownheaders = map (\s -> Unknown s)

showcsdelim (x:[]) = show x
showcsdelim (x:xs) = show x ++ ", " ++ showcsdelim xs


{-
http_url = do
		(a,h,po) <- http_semi
		(p,pa,q) <- px_abs_path +++ return ([],[],"")
		return (Http_Uri a h po p)
-}



