
module Config (configHandler) where


import Conf
import CombUtils
import Http
import HttpStatus
import Mime

import Misc





configHandler :: Handler
configHandler = Handler $ \ev -> case ev of
    Next req -> return (Restart (Report [] req fin), configHandler)
    _ -> return (ev, configHandler)

fin rp req = do
    let req' = req { http_status = (OK, "OK"), http_respdata = Just cnt }
        cnt  = Contents (MimeType "text" "html") (length dt) (Left dt)
	dt   = (concat rp ++ "\n")
    (http_finish req) req'

