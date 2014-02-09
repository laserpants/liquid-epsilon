module Util.String 
    ( pack 
    , unpack 
    ) where

import UHC.Base            ( PackedString, packedStringToString )

pack :: String -> PackedString
pack = __toPackedString

unpack :: PackedString -> String
unpack = packedStringToString

foreign import prim "primStringToPackedString"
    __toPackedString :: String -> PackedString

