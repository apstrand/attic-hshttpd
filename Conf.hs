
module Conf(Conf(..), parseargs, readconf, getPort) where

import IOFuncs
import IO
import Misc

data Conf = Docroot	String
	  | Port	Int
	  | Logwriter	Writer
	  | Logformat	String
	  | Rewriterule	String String
	  | UnknownConf	String String
	  | AddConf	String String
    deriving Show

getPort :: [Conf] -> Maybe Int
getPort ((Port p):_) = Just p
getPort (_:xs) = getPort xs
getPort [] = Nothing

parsearg :: [String] -> IO (Conf,[String])
parsearg (a:b:r) = parsevar dir b >>= \v -> return (v,r)
    where dir = case a of
		"-r" -> "docroot"
		"-p" -> "port"
		"-e" -> "logfile"
		_    -> ""

parseargs :: [String] -> IO [Conf]
parseargs [] = return []
parseargs args = do
    (v,r) <- parsearg args
    cs <- parseargs r
    return (v:cs)

parserule arg = case words arg of
		 [a,b]	->  Rewriterule a b
		 _	->  UnknownConf "rewrite" arg

parsevar "port" arg	 = return $ Port (stoi arg)
parsevar "docroot" arg	 = return $ Docroot arg
parsevar "logformat" arg = return $ Logformat arg
parsevar "rewrite" arg	 = return $ parserule arg
parsevar "logfile" arg	 = do
			     h <- if arg == "-" then return stdErrWriter
                                   else do h <- openFile arg AppendMode
                                           hSetBuffering h LineBuffering
                                           return (toW h)
			     return $ Logwriter h
			   `catch` (\err -> return $ UnknownConf "logfile" (arg ++ " : " ++ show err))
parsevar dir arg	 = return $ UnknownConf dir arg

readconf :: [String] -> IO [Conf]
readconf ls = mapM (\s -> 
	      case words s of
	       (a:b:_)	-> parsevar a b
	       _	-> return $ UnknownConf s ""
	    ) ls

