
module ParseExtra(
	ichar, istring,
	linebr, untilempty,
	schead,
	line,
	doparse,
	URI(..),
	psplit, px_host, px_auth, px_ctl, px_abs_path,
	px_path, px_port, px_query,
	Scheme,
	csdelim
	) where

import Char
import Parser
import Maybe
import Monad

infixr 3 &&&
infixr 2 |||


doparse :: Parser a -> String -> Maybe a
doparse p s = fmap fst (listToMaybe (unP p s))

(|||) a b c = a c || b c 
(&&&) a b c = a c && b c 


ichar c = sat (\x -> toLower x == toLower c)
istring "" = return ""
istring (x:xs) = ichar x >> istring xs >> return (x:xs)
	
crorlf = px_cr ||| px_lf

line = do
	xs <- many (sat (not.crorlf))
	many0 (sat crorlf)
	return xs

linebr = string "\r\n" +++ string "\n"

untilempty :: Parser [String]
untilempty = do
	ls <- many line
	linebr
	return ls


-- Semi-Colon delimited Header...
schead s p c = do
	istring s
	string ": "
	r <- p
	linebr +++ return []
	return (c r)

nbegin = do
	s <- many digit
	c <- item
	l <- line
	return (s,c,l)

nend n = do
	string n
	line

untilnline n ls = do
	do	x <- nend n
		return [x]
    +++
	do	l <- line
		ls <- untilnline n (l:ls)
		return (l:ls)

num_str = do
	(n,c,l) <- nbegin
	case c of
		'-'	->	do
				l2 <- untilnline n [""]
				return (n, l:l2)
		' ' ->	return (n,[l])


type Name = String
type Value = String
type Query = [(Name, Value)]

dquery :: Parser Query
dquery = (do
	name <- urlEnc
	value <- (lit '=' >> urlEnc) +++ return []
	return (name,value)
    ) `sepby` (lit '&')

qsp = lit '+' >> return ' '

urlEnc = many (uchar +++ sat (flip elem ";/?:@") +++ qsp)




-- RFC 1945 3.2


px_char,px_ctl :: Char -> Bool
px_char = (>) (toEnum 128)
px_upalpha = (<=) 'A' &&& (>=) 'Z'
px_loalpha = (<=) 'a' &&& (>=) 'z'

px_alpha = px_upalpha ||| px_loalpha

px_digit = (>=) '9' &&& (<=) '0'
px_ctl = (>) (toEnum 32) ||| (==) (toEnum 127)
px_cr = (==) '\r'
px_lf = (==) '\n'
px_sp = (==) ' '
px_ht = (==) '\t'
px_dq = (==) '\"'

px_hex = isAlphaNum &&& (((>=) 'F') . toUpper)
	
px_tspecial = flip elem "()<>@,;:\\\"{} \t"
px_reserved	= flip elem ";/?:@&=+"
px_extra	= flip elem "!*\\(),_"
px_safe		= flip elem "$-_."
px_unsafe	= px_ctl ||| flip elem " \"#%<>"

px_unreserved = px_alpha ||| px_digit ||| px_safe ||| px_extra ||| px_national

px_national = not . (px_alpha ||| px_digit ||| px_safe ||| px_extra ||| px_reserved ||| px_unsafe)

px_crlf = sat px_cr >> sat px_lf

px_lws = px_crlf >> many (sat (px_sp ||| px_ht))

px_text = many (many0 (sat (not.px_ctl)) +++ px_lws)

px_word = px_token +++ px_qstr

px_token = many (sat (not.px_ctl &&& not.px_tspecial))

px_qstr = do
	sat px_dq
	xs <- many0 (many0 (sat (not.px_ctl &&& not.px_dq)) +++ px_lws)
	sat px_dq
	return (concat xs)

px_comment = do
	lit '('
	xs <- many0 (sat ((/=) '(' &&& (/=) ')'))
	lit ')'
	return xs

px_h2i a
	| a < '9'	= (fromEnum a) - 48
	| a <= 'F'	= (fromEnum a) - 55
	| True		= (fromEnum a) - 87

px_escape :: Parser Char
px_escape = do
	lit '%'
	msn <- sat px_hex
	lsn <- sat px_hex
	return $ chr $ (ord msn) * 16 + (ord lsn)

data Scheme = Sc_http | Sc_ftp | Sc_file | Sc_unknown String deriving Show

data URI = URI {
	uri_scheme	:: 	Scheme,
	uri_auth	::	Maybe (String,String),
	uri_host	::	String,
	uri_port	::	Int,
	uri_path	::	[String],
	uri_params	::	[String],
	uri_query	::	String
	}
	deriving Show

scheme =
		(string "http://" >> return Sc_http)
	+++ (string "ftp://" >> return Sc_ftp)
	+++ (string "file:/" >> return Sc_file)
	+++ (do s <- many0 letter ; lit ':' ; return (Sc_unknown s))

px_port = (do lit ':' ; nat)

net_loc = many0 (pchar +++ lit ';' +++ lit '?')
pchar = uchar +++ sat (flip elem [':','@','&','=','+'])
uchar = sat px_unreserved +++ px_escape

path :: Parser [String]
path = do
	x <- many pchar
	xs <- many0 (lit '/' >> segment)
	return (x:xs)

px_path :: Parser String
px_path = many (pchar +++ lit '/') --  +++ return "/"

segment = many0 pchar

net_path :: Parser (String, [String], [String], String)
net_path = do
	string "//"
	nl <- net_loc
	(p,pa,q) <- (do px_abs_path +++ return ([],[],""))
	return (nl,p,pa,q)

px_abs_path :: Parser ([String], [String], String)
px_abs_path = do
	x <- lit '/'
	xs <- rel_path
	return xs

rel_path :: Parser ([String], [String], String)
rel_path = do
	p <- path +++ return []
	pa <- (lit ';' >> params) +++ return []
	q <- (lit '?' >> px_query) +++ return ""
	return (p,pa,q)

params :: Parser [String]
params = do
	x <- param
	xs <- many0 (lit ';' >> param)
	return (x:xs)

param :: Parser String
param = many0 (pchar +++ lit '/')

px_query :: Parser String
px_query = lit '?' >> many0 (sat (px_unreserved ||| px_reserved ||| (== '%')))

fragment :: Parser String
fragment = many0 (uchar +++ sat px_reserved)


px_auth = (do
		as <- many0 (sat ((/= '@') &&& (/= '/')))
		lit '@'
		return (Just (psplit ':' as)))
		+++ return Nothing

px_host = many (letter +++ digit +++ lit '.' +++ lit '_' +++ lit '-')

psplit c as = (a,b) where 
		(a,b) = case break (==c) as of
				a@(_,[])	->	a
				(a,(_:b))	->  (a,b)

-- RFC 1945 3.3
{-
wkday = string "Mon" >> return 0 
	+++ string "Tue" >> return 1
	+++ string "Wed" >> return 2
	+++ string "Thu" >> return 3
	+++ string "Fri" >> return 4
	+++ string "Sat" >> return 5
	+++ string "Sun" >> return 6
-}
{-
weekday = strings ["Monday", "Tuesday", "Wednesday", "Thursday",
					"Friday", "Saturday", "Sunday"]

month = strings ["Jan", "Feb", "Mar", "Apr", "May", "Jun",
				 "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"]
-}

strings xs = map (\(a,b) -> string a >> return b) (zip xs [0..])
{-
rfc1123_date = do
	wd <- wkday
	string ", "
	d1 <- date1
	string ", "
	t <- time
	string ", GMT"
-}	
	

csdelim = do
		many0 (lit ' ')
		lit ','
		many0 (lit ' ')


