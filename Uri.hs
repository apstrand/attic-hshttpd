
module Uri where

import Parser
import ParseExtra

data Uri = Http_Uri {
		auth	:: Maybe (String,String),
		host	:: String,
		port	:: Int,
		path	:: String,
		query	:: String
	    }
	 | Ftp_Uri {
		auth	:: Maybe (String,String),
		host	:: String,
		port	:: Int,
		path	:: String
	 }
	 | Gen_Uri {
		scheme	:: String,
		host	:: String,
		path	:: String
	 }

instance Read Uri where
    readsPrec _ = unP parse_uri

instance Show Uri where
    showsPrec _ = showuri

read_uri :: String -> Uri
read_uri s = case reads s of
	      [] -> Gen_Uri "" "" s
	      [(u,_)] -> u

showuri (Http_Uri a h pr p q) = showString "http://" . showa a . showString h . showp 80 pr . showString p . showq q
showuri (Ftp_Uri a h pr p) = showString "ftp://" . showa a . showString h . showp 21 pr . showString p
showuri (Gen_Uri s h p)
 | s == "" = showString h . showString p
 | otherwise = showString s . showChar ':' . showString h . showString p

showa Nothing = showString ""
showa (Just (u,p)) = showString u . showChar ':' . showString p . showChar '@'

showp def p
 | def == p  = showString ""
 | otherwise = showChar ':' . showString (show p)

showq "" = showString ""
showq q  = showChar '?' . showString q

parse_uri = parse_http +++ parse_ftp +++ parse_gen

parse_http = do
    string "http://"
    a <- px_auth
    h <- px_host
    pr <- px_port +++ return 80
    p <- px_path +++ return "/"
    q <- px_query +++ return ""
    return (Http_Uri a h pr p q)

parse_ftp = do
    string "ftp://"
    a <- px_auth
    h <- px_host
    pr <- px_port +++ return 21
    p <- px_path
    return (Ftp_Uri a h pr p)

parse_gen = do
    s <- word
    lit ':'
    h <- px_host
    p <- px_path
    return (Gen_Uri s h p)


test = ["http://user:pass@host.nowhere.net:99/some/where/file.foo?this%20is&a=test",
	"http://zarquon",
	"http://med/test.html",
	"ftp://ftp:pw@some.where.org/pub/Linux/kernel/",
	"news:comp.lang.functional"
       ]

--main = mapM_ (print.unP parse_uri) test

