
module Rewrite (
    checkurl,
    rewriter,
    reqMatcher
    ) where

import List
import Maybe

import Http
import HttpStatus
import Conf
import IOFuncs
import CombUtils
import Misc

import RX

type Rules = [(RX,String)]

rewriter :: Handler
rewriter = Handler (rewriter' [])

rewriter' rules c@(Configure conf) =  return (c, Handler $ rewriter' ((compileRX conf)++rules))
rewriter' rules req@(Next hreq) = return (req', rewriter)
    where req' = maybe req (\p -> Restart (Next (hreq { http_path = p })))
		    (findmatchrx rules (http_path hreq))
rewriter' _ ev = return (ev, rewriter)

checkurl = simpleMaybeHandler "checkurl" (\req -> 
	not $ any (isPrefixOf "..") (tails (http_path req)))

findmatchrx :: [(RX,String)] -> String -> Maybe String
findmatchrx rs str = fmap snd $ find ((`match` str) . fst) rs


compileRX :: Conf -> [(RX,String)]
compileRX (Rewriterule rule arg) = maybe [] (\c -> [(c,arg)]) $ compile rule 
compileRX _ = []

reqMatcher :: [String] -> String -> Handler -> Handler
reqMatcher rs name handler = maybeHandler updstr mp (foldr updstr [] (map Left rs)) name handler
    where mp = \req s -> any (`match` (http_path req)) (map snd s)
	  updstr = either (maybe id (:) . (\a-> fmap ((,) a) (compile a)) ) (cK id)


