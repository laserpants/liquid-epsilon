module Util.State 
    ( IsState(..)
    , _readState
    , _setState
    ) where

import UHC.Ptr
import Util.DOM              ( window )
import Util.String           ( pack )

class IsState a where
    readState :: IO a
    setState  :: a -> IO ()
    initState :: a
    readState = _readState ""
    setState  = _setState  ""

_readState :: (IsState a) => String -> IO a
_readState s = do
    let p = pack $ "__state__" ++ s
    b <- __checkProperty p
    case b of
      0 -> return initState
      _ -> __readState p

foreign import js "window[%1]"
    __readState :: PackedString -> IO a

foreign import js "window.hasOwnProperty(%*)"
    __checkProperty :: PackedString -> IO Int

_setState :: (IsState a) => String -> a -> IO ()
_setState n s = window >>= __set (pack $ "__state__" ++ n) s >> return ()

foreign import prim "primSetAttr"
    __set :: PackedString -> a -> Ptr p -> IO (Ptr p)

