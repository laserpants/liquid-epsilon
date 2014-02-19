module Util.State 
    ( _readState
    , _setState
    ) where

import UHC.Ptr
import Util.DOM              ( window )
import Util.String           ( pack )

_readState :: String -> IO a
_readState s = __readState $ pack $ "__state__" ++ s

foreign import js "window.%1"
    __readState :: PackedString -> IO a

_setState :: String -> a -> IO ()
_setState n s = window >>= __set (pack $ "__state__" ++ n) s >> return ()

foreign import prim "primSetAttr"
    __set :: PackedString -> a -> Ptr p -> IO (Ptr p)

