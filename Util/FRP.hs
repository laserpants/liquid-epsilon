module Util.FRP 
    ( module Control.Applicative
    , Signal
    , attach
    , _bindSignal
    , _bindUnitSignal
    , onSignal
    , inputValue
    , inputValue'
    , clickSignal
    , mouseSignal
    , scrollXSignal
    , scrollYSignal
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
    __addEventListener e (pack ev) $ __wrap1
        $ \obj -> do
            -- Add the event object to the element
            setEventProperty obj e
            v >>= id
    attach (Signal xs v)
  where setEventProperty = __set (pack "___e")

-- | Create a signal by binding a specific event to a property on the given element.
_bindSignal :: String      -- ^ The name of the event, e.g., 'change'
            -> String      -- ^ Property to read from when the event is triggered.
            -> Element     -- ^ A DOM element
            -> Signal a
_bindSignal ev prop e =
    let s = Signal { event = [(e, ev)]
                   , poll  = return e }
    in  __prop <$> s <*> pure (pack prop)

-- | Create a signal of unit type, i.e., one which is not associated with a property, 
-- but instead emits a simple () value.
_bindUnitSignal :: String      -- ^ The name of the event, e.g., 'click'
                -> Element     -- ^ A HTML DOM element
                -> Signal ()
_bindUnitSignal ev e = Signal { event = [(e, ev)], poll = return () }

-- | Promote an IO action to a signal with the help of another signal.
onSignal :: Signal a        -- ^ The "activating" signal
         -> IO b            -- ^ An action to trigger when the signal emits.
         -> Signal (IO b)
onSignal s a = fmap (\_ f -> f) s <*> pure a

-- | Creates a signal for the value of an input field.
inputValue :: Element -> Signal String
inputValue = fmap unpack . inputValue' 

inputValue' :: Element -> Signal PackedString
inputValue' e = 
    let s = Signal { event = [(e, "input")]
                   , poll  = return e }
    in  fmap __val s 

-- | A signal that reacts to click events on an element.
clickSignal :: Element -> Signal ()
clickSignal = _bindUnitSignal "click"

-- | Mouse cursor position signal.
mouseSignal :: Signal (Int, Int)
mouseSignal = let d = __document
              in  fmap mouse $ Signal { event = [(d, "mousemove")]
                                      , poll  = return d }
  where mouse :: Element -> (Int, Int)
        mouse e = case __hasEventProperty e == 1 of
                    True  -> (__clientX e, __clientY e)
                    False -> (0, 0)

-- | Window scroll X position signal.
scrollXSignal :: Signal Int
scrollXSignal = _bindSignal "scroll" "scrollX" __window

-- | Window scroll Y position signal.
scrollYSignal :: Signal Int
scrollYSignal = _bindSignal "scroll" "scrollY" __window

foreign import js "%1.value"
    __val :: Element -> PackedString

foreign import js "%1[%2]"
    __prop :: Element -> PackedString -> a

foreign import js "wrapper"
    __wrap1 :: (a -> b) -> FunPtr (a -> b)

foreign import js "%1.addEventListener(%2, %3, false)"
    __addEventListener :: Element -> PackedString -> FunPtr a -> IO ()

foreign import js "document"
    __document :: Element

foreign import js "window"
    __window :: Element

foreign import prim "primSetAttr"
    __set :: PackedString -> a -> Ptr p -> IO (Ptr p)

foreign import js "%1.hasOwnProperty('___e')"
    __hasEventProperty :: Element -> Int

foreign import js "%1.___e['clientX']"
    __clientX :: Element -> Int

foreign import js "%1.___e['clientY']"
    __clientY :: Element -> Int

