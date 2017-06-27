
module ServerParsed where

import Directory(removeFile)

import CombUtils
import Http
import HttpStatus
import IOLib
import Local
import Mime
import Misc

parseit :: String -> String -> [String] -> [String] -> ([String], [String])
parseit [] acc cs ps = ((cs ++ [reverse acc]), ps)
parseit ('<':'!':'-':'-':'#':'e':'x':'e':'c':' ':'c':'g':'i':'=':'"':xs) acc cs ps = 
    let (path,xs') = span (/='"') xs 
	rest = dropWhile (/='>') xs'
    in parseit (tail rest) "" (cs ++ [reverse acc]) (ps ++ [path])
parseit (x:xs) acc cs ps = parseit xs (x:acc) cs ps

readNparse :: Contents -> IO ([String], [String])
readNparse (Contents mime len (Right rd)) = do
    cnts <- readn rd len
    close rd
    return (parseit cnts "" [] [])

--shtmlFinisher :: Output w => Finisher -> w -> String -> [String] -> [String] -> Finisher
shtmlFinisher fin wr wp [c] [] req = do
    ifM (http_respdata req) (\(Contents _ _ (Right rd)) -> transfer rd wr)
    write wr c
    size <- file_size wp
    s <- open_file wp Reading
    removeFile wp
    fin (req { http_respdata = Just (Contents unknown size (Right s)) } )

shtmlFinisher fin wr wp (c:cs) (p:ps) req = do
    write wr c
    ifM (http_respdata req) (\(Contents _ _ (Right rd)) -> transfer rd wr)
    return (Left (req { http_path = p, 
			http_finish = shtmlFinisher fin wr wp cs ps } ))

shtmlFinisher _ wr wp cs ps req = return (Right [])

handleshtml = confHandler "./" updDocroot $ \root req -> do
     return  $ Next $ req { http_finish = (\req' -> do
	 case (fst (http_status req'), http_respdata req') of
	    (OK, Just cn) -> do
		(cs,ps) <- readNparse cn
		tmp <- tempname
		fd <- open_file tmp Writing
		(shtmlFinisher (http_finish req) fd tmp cs ps) (req' { http_respdata = Nothing } )
	    _ -> do
		    (http_finish req) req'
		    return (Right [])
	   ) }

