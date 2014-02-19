module Util.Base 
    ( module Util.Router 
    , addScript
    , addScripts
    , _readState
    , _setState
    ) where

import UHC.Ptr
import Util.DOM
import Util.Router           ( wrap, onLoad, onReady )
import Util.String           ( pack )

addScripts :: [String] -> IO ()
addScripts xs = wrap 
    (documentBody >>= sequence . flip map xs . addOne) 
    >>= onReady
  where addOne :: Element -> String -> IO ()
        addOne body src = do 
            s <- createElement "script"
            setAttribute s "type" "text/javascript"
            setAttribute s "src" src
            s ~> body

addScript :: String -> IO ()
addScript s = addScripts [s]

_readState :: String -> IO a
_readState s = __readState $ pack $ "__state__" ++ s

foreign import js "window.%1"
    __readState :: PackedString -> IO a

_setState :: String -> a -> IO ()
_setState n s = window >>= __set (pack $ "__state__" ++ n) s >> return ()

foreign import prim "primSetAttr"
    __set :: PackedString -> a -> Ptr p -> IO (Ptr p)

