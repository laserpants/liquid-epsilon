module Util.HTML.PS.Attributes where

import Util.HTML.PS

action, align, alt, autocomplete, background, border, charset, checked, _class, cols, colspan, content, enctype, for, height, href, http_equiv, _id, maxlength, method, name, placeholder, role, rows, rowspan, selected, size, src, style, tabindex, target, title, _type, value, width :: PackedString -> Attribute

action         = makeAttr __attr_action
align          = makeAttr __attr_align
alt            = makeAttr __attr_alt
autocomplete   = makeAttr __attr_autocomplete
background     = makeAttr __attr_background
border         = makeAttr __attr_border
charset        = makeAttr __attr_charset
checked        = makeAttr __attr_checked
_class         = makeAttr __attr_class
cols           = makeAttr __attr_cols
colspan        = makeAttr __attr_colspan
content        = makeAttr __attr_content
enctype        = makeAttr __attr_enctype
for            = makeAttr __attr_for
height         = makeAttr __attr_height
href           = makeAttr __attr_href
http_equiv     = makeAttr __attr_http_equiv
_id            = makeAttr __attr_id
maxlength      = makeAttr __attr_maxlength
method         = makeAttr __attr_method
name           = makeAttr __attr_name
placeholder    = makeAttr __attr_placeholder
role           = makeAttr __attr_role
rows           = makeAttr __attr_rows
rowspan        = makeAttr __attr_rowspan
selected       = makeAttr __attr_selected
size           = makeAttr __attr_size
src            = makeAttr __attr_src
style          = makeAttr __attr_style
tabindex       = makeAttr __attr_tabindex
target         = makeAttr __attr_target
title          = makeAttr __attr_title
_type          = makeAttr __attr_type
value          = makeAttr __attr_value
width          = makeAttr __attr_width

foreign import js "String('action')"          __attr_action        :: PackedString
foreign import js "String('align')"           __attr_align         :: PackedString
foreign import js "String('alt')"             __attr_alt           :: PackedString
foreign import js "String('autocomplete')"    __attr_autocomplete  :: PackedString
foreign import js "String('background')"      __attr_background    :: PackedString
foreign import js "String('border')"          __attr_border        :: PackedString
foreign import js "String('charset')"         __attr_charset       :: PackedString
foreign import js "String('checked')"         __attr_checked       :: PackedString
foreign import js "String('class')"           __attr_class         :: PackedString
foreign import js "String('cols')"            __attr_cols          :: PackedString
foreign import js "String('colspan')"         __attr_colspan       :: PackedString
foreign import js "String('content')"         __attr_content       :: PackedString
foreign import js "String('enctype')"         __attr_enctype       :: PackedString
foreign import js "String('for')"             __attr_for           :: PackedString
foreign import js "String('height')"          __attr_height        :: PackedString
foreign import js "String('href')"            __attr_href          :: PackedString
foreign import js "String('http-equiv')"      __attr_http_equiv    :: PackedString
foreign import js "String('id')"              __attr_id            :: PackedString
foreign import js "String('maxlength')"       __attr_maxlength     :: PackedString
foreign import js "String('method')"          __attr_method        :: PackedString
foreign import js "String('name')"            __attr_name          :: PackedString
foreign import js "String('placeholder')"     __attr_placeholder   :: PackedString
foreign import js "String('role')"            __attr_role          :: PackedString
foreign import js "String('rows')"            __attr_rows          :: PackedString
foreign import js "String('rowspan')"         __attr_rowspan       :: PackedString
foreign import js "String('selected')"        __attr_selected      :: PackedString
foreign import js "String('size')"            __attr_size          :: PackedString
foreign import js "String('src')"             __attr_src           :: PackedString
foreign import js "String('style')"           __attr_style         :: PackedString
foreign import js "String('tabindex')"        __attr_tabindex      :: PackedString
foreign import js "String('target')"          __attr_target        :: PackedString
foreign import js "String('title')"           __attr_title         :: PackedString
foreign import js "String('type')"            __attr_type          :: PackedString
foreign import js "String('value')"           __attr_value         :: PackedString
foreign import js "String('width')"           __attr_width         :: PackedString

