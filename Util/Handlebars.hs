module Util.Handlebars 
    ( compile
    , render
    , render'
    ) where

import UHC.Ptr
import Util.JSON
import Util.String

compile :: forall a. String -> IO (Ptr a -> IO PackedString)
compile source = __compile (pack source) >>= return . __mkFun

render :: (ToJSON a) => (Ptr a -> IO PackedString) -> a -> IO String
render f obj = render' f obj >>= return . unpack 

render' :: (ToJSON a) => (Ptr a -> IO PackedString) -> a -> IO PackedString
render' f obj = (__parse $ pack $ toStr obj) >>= f

foreign import js "Handlebars.compile(%*)"
    __compile :: PackedString -> IO (FunPtr (Ptr a -> IO PackedString))

foreign import js "dynamic"
    __mkFun :: FunPtr (Ptr a -> IO PackedString) -> Ptr a -> IO PackedString

foreign import js "JSON.parse(%*)" 
    __parse :: PackedString -> IO (Ptr a)

