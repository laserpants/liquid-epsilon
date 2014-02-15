module Util.HTML.Operators where

import Util.HTML.Attributes        ( _type, _class, _id )
import Util.HTML                   ( Html, Attributable, (!) )

(!@), (!&), (!:) :: Attributable h => h -> String -> h
a !@ b = a ! _type b
a !& b = a ! _class b
a !: b = a ! _id b

