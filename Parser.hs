
-- A simple monadic parser

-- Some ideas from [Monadic parser combinators, Graham Hutton and Erik Meijer]
-- http://www.cs.nott.ac.uk/~gmh/bib.html#monparsing



module Parser(Parser(P), unP, item, (+++), (?>), letter, digit, lit, sat,
	  many,many0,word,sepby,nat,space,spaces,one,string,module Monad) where 

import Char
import Monad

import Misc

infixr 5 +++

newtype Parser a = P (String -> [(a,String)])
unP (P a) = a

instance Functor Parser where
    fmap f (P p) = P (\s -> [(f v, out) | (v,out) <- p s])

instance Monad Parser where
    return a	= P (\x -> [(a,x)])
    m >>= f	= P (\x -> [(b,z) | (a,y) <- (unP m) x, (b,z) <- (unP (f a)) y])

instance MonadPlus Parser where
    mzero		= P (\s -> [])
    mplus (P p) (P q)	= P (\x -> p x ++ q x)

-----------------------------------------------------------



first :: Parser a -> Parser a
first (P p) = P (\x -> case p x of
	[]	-> []
	(x:_)	-> [x]
    )

(+++) :: Parser a -> Parser a -> Parser a
m +++ n = first (m `mplus` n)

(?>) :: Parser a -> (a -> Bool) -> Parser a
m ?> p = m >>= \x -> if p x then return x else mzero



many m = liftM2 (:) m (many0 m)
many0 m = many m `mplus` return []

one :: Parser a -> Parser [a]
one m = m >>= (\a -> return [a])

sepby :: Parser a -> Parser b -> Parser [a]
sepby p sep = p >>= \a -> sep >>= \_ -> p >>= \b -> return ([a] ++ [b])


-----------------------------------------------------------


item :: Parser Char
item = P (\inp -> case inp of
	[]	-> []
	(x:xs)	-> [(x,xs)]
    )

sat f = item ?> f

letter :: Parser Char
letter = item ?> isAlpha

string "" = return ""
string (x:xs) = lit x >> string xs >> return (x:xs)

digit :: Parser Char
digit = item ?> isDigit

lit :: Char -> Parser Char
lit c = item ?> \a -> a == c

word :: Parser String
word = many letter

space :: Parser Char
space = item ?> isSpace

spaces :: Parser String
spaces = many space

nat :: Parser Int
nat = many digit >>= \d -> return (tonum (d))

tonum :: [Char] -> Int
tonum = (foldl (\a b -> 10 * a+b) 0) . map digitToInt

