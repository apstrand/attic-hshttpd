
module Mime(
    MimeType(..),
    unknown,
    mime,
    mimep,
    mimeguesser,
    mimeform
    )where

import Parser
import ParseExtra

data MimeType = MimeType String String deriving Eq
instance Show MimeType where
	show (MimeType a b) = a ++ "/" ++ b

unknown = MimeType "application" "octet-stream"

mime :: String -> MimeType
mime s = maybe unknown id (doparse mimep s)

mimep :: Parser MimeType
mimep = do
	maj <- many letter
	lit '/'
	min <- many (letter +++ lit '-')
	return (MimeType maj min)

mimeform = MimeType "application" "x-www-form-urlencoded"

mimeguesser :: String -> MimeType
mimeguesser p = guess (reverse (takeWhile (/='.') (reverse p)))
    where guess "txt"	= MimeType "text" "plain"
	  guess "hs"	= MimeType "text" "plain"
	  guess "html"	= MimeType "text" "html"
	  guess "shtml"	= MimeType "text" "html"
	  guess "htm"	= MimeType "text" "html"
	  guess "gif"	= MimeType "image" "gif"
	  guess "jpeg"	= MimeType "image" "jpeg"
	  guess "jpg"	= MimeType "image" "jpeg"
	  guess "png"	= MimeType "image" "png"
	  guess _	= unknown


