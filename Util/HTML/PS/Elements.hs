module Util.HTML.PS.Elements where

import Util.HTML.PS

a, article, aside, b, blockquote, body, button, canvas, code, dd, div, dl, dt, em, fieldset, footer, form, h1, h2, h3, h4, h5, h6, _head, html, i, _label, menu, nav, ol, option, p, pre, q, section, select, span, sub, sup, table, tbody, td, textarea, tfoot, thead, title, tr, ul, video :: Html -> Html

a          = makePar __el_a
article    = makePar __el_article
aside      = makePar __el_aside
b          = makePar __el_b
blockquote = makePar __el_blockquote
body       = makePar __el_body
button     = makePar __el_button 
canvas     = makePar __el_canvas
code       = makePar __el_code
dd         = makePar __el_dd
div        = makePar __el_div
dl         = makePar __el_dl
dt         = makePar __el_dt
em         = makePar __el_em
fieldset   = makePar __el_fieldset
footer     = makePar __el_footer
form       = makePar __el_form
h1         = makePar __el_h1
h2         = makePar __el_h2
h3         = makePar __el_h3
h4         = makePar __el_h4
h5         = makePar __el_h5
h6         = makePar __el_h6
_head      = makePar __el_head
html       = makePar __el_html
i          = makePar __el_i
_label     = makePar __el_label
li         = makePar __el_li
menu       = makePar __el_menu
nav        = makePar __el_nav
ol         = makePar __el_ol
option     = makePar __el_option
p          = makePar __el_p
pre        = makePar __el_pre
q          = makePar __el_q
section    = makePar __el_section
select     = makePar __el_select
span       = makePar __el_span
sub        = makePar __el_sub
sup        = makePar __el_sup
table      = makePar __el_table
tbody      = makePar __el_tbody
td         = makePar __el_td
textarea   = makePar __el_textarea
tfoot      = makePar __el_tfoot
thead      = makePar __el_thead
title      = makePar __el_title
tr         = makePar __el_tr
ul         = makePar __el_ul
video      = makePar __el_video

docTypeHtml :: Html -> Html
docTypeHtml = (>>) (makeLeaf __doctype) . html 

docTypeHtmlLang :: PackedString -> Html -> Html
docTypeHtmlLang lang = (>>) (makeLeaf __doctype) . (html ! attr lang) 
  where attr = makeAttr __lang

br, col, hr, img, input, nbsp, textarea' :: Html

br         = makeLeaf __el_br
col        = makeLeaf __el_col
hr         = makeLeaf __el_hr
img        = makeLeaf __el_img
input      = makeLeaf __el_input

nbsp       = leaf __nbsp __nbsp_ __blank
textarea'  = textarea $< __blank

foreign import js "String()"                __blank         :: PackedString
foreign import js "String('nbsp')"          __nbsp          :: PackedString
foreign import js "String('&nbsp;')"        __nbsp_         :: PackedString
foreign import js "String('lang')"          __lang          :: PackedString
foreign import js "String('!DOCTYPE html')" __doctype       :: PackedString
foreign import js "String('a')"             __el_a          :: PackedString
foreign import js "String('article')"       __el_article    :: PackedString
foreign import js "String('aside')"         __el_aside      :: PackedString
foreign import js "String('b')"             __el_b          :: PackedString
foreign import js "String('blockquote')"    __el_blockquote :: PackedString
foreign import js "String('body')"          __el_body       :: PackedString
foreign import js "String('button')"        __el_button     :: PackedString
foreign import js "String('canvas')"        __el_canvas     :: PackedString
foreign import js "String('code')"          __el_code       :: PackedString
foreign import js "String('dd')"            __el_dd         :: PackedString
foreign import js "String('div')"           __el_div        :: PackedString
foreign import js "String('dl')"            __el_dl         :: PackedString
foreign import js "String('dt')"            __el_dt         :: PackedString
foreign import js "String('em')"            __el_em         :: PackedString
foreign import js "String('fieldset')"      __el_fieldset   :: PackedString
foreign import js "String('footer')"        __el_footer     :: PackedString
foreign import js "String('form')"          __el_form       :: PackedString
foreign import js "String('h1')"            __el_h1         :: PackedString
foreign import js "String('h2')"            __el_h2         :: PackedString
foreign import js "String('h3')"            __el_h3         :: PackedString
foreign import js "String('h4')"            __el_h4         :: PackedString
foreign import js "String('h5')"            __el_h5         :: PackedString
foreign import js "String('h6')"            __el_h6         :: PackedString
foreign import js "String('head')"          __el_head       :: PackedString
foreign import js "String('html')"          __el_html       :: PackedString
foreign import js "String('i')"             __el_i          :: PackedString
foreign import js "String('label')"         __el_label      :: PackedString
foreign import js "String('li')"            __el_li         :: PackedString
foreign import js "String('menu')"          __el_menu       :: PackedString
foreign import js "String('nav')"           __el_nav        :: PackedString
foreign import js "String('ol')"            __el_ol         :: PackedString
foreign import js "String('option')"        __el_option     :: PackedString
foreign import js "String('p')"             __el_p          :: PackedString
foreign import js "String('pre')"           __el_pre        :: PackedString
foreign import js "String('q')"             __el_q          :: PackedString
foreign import js "String('section')"       __el_section    :: PackedString
foreign import js "String('select')"        __el_select     :: PackedString
foreign import js "String('span')"          __el_span       :: PackedString
foreign import js "String('sub')"           __el_sub        :: PackedString
foreign import js "String('sup')"           __el_sup        :: PackedString
foreign import js "String('table')"         __el_table      :: PackedString
foreign import js "String('tbody')"         __el_tbody      :: PackedString
foreign import js "String('td')"            __el_td         :: PackedString
foreign import js "String('textarea')"      __el_textarea   :: PackedString
foreign import js "String('tfoot')"         __el_tfoot      :: PackedString
foreign import js "String('thead')"         __el_thead      :: PackedString
foreign import js "String('title')"         __el_title      :: PackedString
foreign import js "String('tr')"            __el_tr         :: PackedString
foreign import js "String('ul')"            __el_ul         :: PackedString
foreign import js "String('video')"         __el_video      :: PackedString
foreign import js "String('br')"            __el_br         :: PackedString
foreign import js "String('col')"           __el_col        :: PackedString
foreign import js "String('hr')"            __el_hr         :: PackedString
foreign import js "String('img')"           __el_img        :: PackedString
foreign import js "String('input')"         __el_input      :: PackedString

