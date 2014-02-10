module Util.FRP 
    ( module Control.Applicative
    , Signal
    , attach
    , inputValue
    , inputValue'
    ) where

import Control.Applicative         ( Applicative, pure, (<$>), (<*>) )
import Control.Monad               ( liftM )
import UHC.Ptr
import Util.DOM
import Util.String

data Signal a = Signal
    { event :: [(Element, String)]
    , poll  :: IO a
    }

instance Functor Signal where
    fmap f (Signal s v) = Signal s (liftM f v)
              
instance Applicative Signal where
    pure a = Signal
        { event = []
        , poll  = return a
        }
    Signal a p1 <*> Signal b p2 = Signal
        { event = a ++ b
        , poll  = p1 <*> p2
        }

attach :: Signal (IO a) -> IO ()
attach (Signal [] v) = v >>= id >> return ()
attach (Signal (x:xs) v) = do
    let (e, ev) = x
    __addEventListener e (pack ev) $ __wrap 
        $ v >>= id
    attach (Signal xs v)

inputValue :: Element -> Signal String
inputValue = fmap unpack . inputValue' 

inputValue' :: Element -> Signal PackedString
inputValue' e = 
    let s = Signal { event = [(e, "input")]
                   , poll  = return e }
    in  fmap __val s 

foreign import js "%1.value"
    __val :: Element -> PackedString

foreign import js "wrapper"
    __wrap :: a -> FunPtr a

foreign import js "%1.addEventListener(%2, %3, false)"
    __addEventListener :: Element -> PackedString -> FunPtr a -> IO ()
 
