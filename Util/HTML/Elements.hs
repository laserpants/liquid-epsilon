module Util.HTML.Elements where

import Util.HTML

a, article, aside, b, blockquote, body, button, canvas, code, dd, div, dl, dt, em, fieldset, footer, form, h1, h2, h3, h4, h5, h6, _head, html, i, _label, menu, nav, ol, option, p, pre, q, section, select, span, sub, sup, table, tbody, td, textarea, tfoot, thead, title, tr, ul, video :: Html -> Html

a          = makePar "a"
article    = makePar "article"
aside      = makePar "aside"
b          = makePar "b"
blockquote = makePar "blockquote"
body       = makePar "body"
button     = makePar "button" 
canvas     = makePar "canvas"
code       = makePar "code"
dd         = makePar "dd"
div        = makePar "div"
dl         = makePar "dl"
dt         = makePar "dt"
em         = makePar "em"
fieldset   = makePar "fieldset"
footer     = makePar "footer"
form       = makePar "form"
h1         = makePar "h1"
h2         = makePar "h2"
h3         = makePar "h3"
h4         = makePar "h4"
h5         = makePar "h5"
h6         = makePar "h6"
_head      = makePar "head"
html       = makePar "html"
i          = makePar "i"
_label     = makePar "label"
li         = makePar "li"
menu       = makePar "menu"
nav        = makePar "nav"
ol         = makePar "ol"
option     = makePar "option"
p          = makePar "p"
pre        = makePar "pre"
q          = makePar "q"
section    = makePar "section"
select     = makePar "select"
span       = makePar "span"
sub        = makePar "sub"
sup        = makePar "sup"
table      = makePar "table"
tbody      = makePar "tbody"
td         = makePar "td"
textarea   = makePar "textarea"
tfoot      = makePar "tfoot"
thead      = makePar "thead"
title      = makePar "title"
tr         = makePar "tr"
ul         = makePar "ul"
video      = makePar "video"

docTypeHtml :: Html -> Html
docTypeHtml = (>>) (makeLeaf "!DOCTYPE html") . html 

docTypeHtmlLang :: String -> Html -> Html
docTypeHtmlLang lang = (>>) (makeLeaf "!DOCTYPE html") . (html ! attr lang) 
  where attr = makeAttr "lang"

br, col, hr, img, input, nbsp, textarea' :: Html

br         = makeLeaf "br"
col        = makeLeaf "col"
hr         = makeLeaf "hr"
img        = makeLeaf "img"
input      = makeLeaf "input"

nbsp       = leaf "nbsp" "&nbsp;" ""
textarea'  = textarea $< ""

