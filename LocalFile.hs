
module LocalFile(fileHandler, handleDir) where

import IO

import Monad

import Html
import Http
import CombUtils
import HttpStatus

import IOFuncs

import Mime
import Local
import Directory

handleDir = confHandler "./" updDocroot $ \root req -> do
    let path = http_path req
	abspath = root ++ path
    ft <- file_type abspath
    if ft /= Directory then return (Next req)
     else do
	    let pathidx = path ++ "/index.html"
	    ft <- file_type (root ++ pathidx)
	    if ft == File then return (Restart (Next (req { http_path = pathidx })))
	     else do
                    cts <- getDirectoryContents abspath
                    let html = format_dirlist path (drop 2 cts)
                        m = MimeType "text" "html"
                        s = length html
                    return $ finit (R_OK, "OK") [] (Just (Contents m s (Left html))) req


fileHandler = confHandler "./" updDocroot handleFile

handleFile root req = do
    let p = root ++ (http_path req)
    size <- file_size p
    s <- open_file p Reading
    return (respFile size (mimeguesser p) (SR s) req)
  `catch`
    (\err -> print "Err!" >> return (fileErr (http_path req) err req))



format_dirlist path cts = 
 page (title path) $ 
    h1 ("Contents of: " ++ path) ++
    (tag_ "ul" $ 
     concatMap (\e -> tag_ "li" (a (path ++ e) e) ++ "\n") cts)

