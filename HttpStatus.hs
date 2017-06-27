
module HttpStatus(
    StatusCode(..),
    code2num
    ) where

data StatusCode = R_OK
		| R_Created
		| R_Accepted
		| R_NoContent
		| R_MovedPerm
		| R_MovedTemp
		| R_NotModified
		| R_BadReq
		| R_Unauthorized
		| R_Forbidden
		| R_NotFound
		| R_IntErr
		| R_NotImpl
		| R_BadGateway
		| R_ServUnavail
		| R_NotHandled
		| R_UnParsed
		deriving Eq

instance Show StatusCode where
	show	R_OK		= "OK"
	show	R_Created	= "Created"
	show	R_Accepted	= "Accepted"
	show	R_NoContent	= "No Content"
	show	R_MovedPerm	= "Moved Permanently"
	show	R_MovedTemp	= "Moved Temporarily"
	show	R_NotModified	= "Not Modified"
	show	R_BadReq	= "Bad Request"
	show	R_Unauthorized	= "Unauthorized"
	show	R_Forbidden	= "Forbidden"
	show	R_NotFound	= "Not Found"
	show	R_IntErr	= "Internal Server Error"
	show	R_NotImpl	= "Not Implemented"
	show	R_BadGateway	= "Bad Gateway"
	show	R_ServUnavail	= "Service Unavailable"
	show	R_UnParsed	= "Unparsed contents"
	show	R_NotHandled	= "Not handled"
    

code2num c = case c of
	R_OK		-> 200
	R_Created	-> 201
	R_Accepted	-> 202
	R_NoContent	-> 204
	R_MovedPerm	-> 301
	R_MovedTemp	-> 302
	R_NotModified	-> 304
	R_BadReq	-> 400
	R_Unauthorized	-> 401
	R_Forbidden	-> 403
	R_NotFound	-> 404
	R_IntErr	-> 500
	R_NotImpl	-> 501
	R_BadGateway	-> 502
	R_ServUnavail	-> 503
	R_NotHandled	-> 003
	R_UnParsed	-> 004

