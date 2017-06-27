
module CombUtils((->-), simpleHandler, maybeHandler, confHandler, simpleMaybeHandler) where

import Monad

import Misc
import Conf
import Http

infixl 5 ->-

(->-) :: Handler -> Handler -> Handler
(->-) (Handler h1) (Handler h2) = Handler (\ev -> 
    case ev of
     Restart _ -> return (ev, Handler h1 ->- Handler h2)
     Next _ -> h1 ev >>= \(ev',h1') -> case ev' of
				     Restart _ -> return (ev', h1' ->- Handler h2)
				     Next _ -> h2 ev' >>= \(ev'', h2') -> return (ev'', h1' ->- h2')
				     Wait _ -> return (ev', h1' ->- Handler h2)
				     _ -> error "generated an invalid event"
     Configure _ -> h1 ev >>= \(_,h1') -> h2 ev >>= \(_,h2') -> return (ev, h1' ->- h2')
     Wait _ -> return (ev, Handler h1 ->- Handler h2)
     _ -> h1 ev >>= \(ev',h1') -> h2 ev' >>= \(ev'',h2') -> return (ev'', h1' ->- h2')
    )

simpleHandler :: (ReqData -> IO ReqData) -> Handler
simpleHandler f = confHandler () (\_ _ -> ()) (\_ req -> liftM Next $ f req)

runH :: (Handler -> Handler) -> Handler -> Event -> IO (Event, Handler)
runH ht h ev = runHandler h ev >>= \(ev', h') -> return (ev', ht h')

confHandler :: a -> (a -> Conf -> a) -> (a -> ReqData -> IO Event) -> Handler
confHandler confdef confupd handler = Handler (confH confdef)
    where confH conf c@(Configure conf') = return (c, Handler $ confH (confupd conf conf'))
	  confH conf (Next req) = handler conf req >>= \req' -> return (req', Handler $ confH conf)
	  confH conf ev =  return (ev, Handler $ confH conf)


maybeHandler
 :: (Either String Conf -> s -> s)  -- State updater
 -> (ReqData -> s -> Bool)	    -- Request match function
 -> s				    -- Initial state
 -> String			    -- Name
 -> Handler			    -- Subhandler
 -> Handler
maybeHandler upd m st name h = Handler maybeHandler'
 where 
  maybeHandler' :: HandlerT
  
  maybeHandler' ev@(Configure (AddConf id str))
   | id == name = ret ev st h
   | otherwise  = run ev h
  
  maybeHandler' ev@(Next req)
   | m req st  = log req "match" >> run ev h
   | otherwise = log req "no match" >> ret ev st h
  
  maybeHandler' ev = run ev h
  
  run ev h = runHandler h ev >>= \(ev',h') -> case ev' of
    Report xs req fin -> ret (Report (name:xs) req fin) st h'
    _ -> ret ev' st h'

  ret ev st' h' = return (ev, maybeHandler upd m st' name h')
  log req s = return ()
--  log req s = putStrLn (name ++ ": " ++ http_path req ++ " -> " ++ s)

simpleMaybeHandler :: String -> (ReqData -> Bool) -> Handler -> Handler
simpleMaybeHandler name p = maybeHandler (cK id) (\r _ -> p r) () name

