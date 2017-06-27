
module Html where


tag_ t cts = tag t [] cts
tag t as cts = "<" ++ t ++ attrs as ++ ">" ++ cts ++ "</" ++ t ++ ">"

stag t as = "<" ++ t ++ attrs as ++ ">"

attrs [] = ""
attrs ((a,v):as) = " " ++ a ++ "=\"" ++ v ++ "\"" ++ attrs as

html_4_0_transitional = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \"http://www.w3.org/TR/REC-html40/loose.dtd\">"
html_4_01_strict = "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">"

page :: String -> String -> String
page h b
    = html_4_0_transitional ++ "\n"
    ++ html (header (meta enc ++ h ++ "\n") ++ body [("bgcolor","white")] b ++ "\n")

meta as = stag "meta" as ++ "\n"

enc = [("http-equiv", "Content-type"),
       ("content", "text/html; charset=iso-8859-1")]

html = tag_ "html"
header = tag_ "head"
body = tag "body"
title = tag_ "title"
p = tag_ "p"
a url = tag "a" [("href",url)]
h1 = tag_ "h1"

