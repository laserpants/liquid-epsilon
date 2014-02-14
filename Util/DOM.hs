module Util.DOM
    ( Element
    , createElement
    , createElement'
    , setInnerHtml
    , setInnerHtml'
    , documentWrite
    , documentWrite'
    , documentBody
    , window
    , appendChild
    , setAttribute
    , setAttribute'
    , setValue
    , setValue'
    , toString
    , toString'
    , innerHtml
    , innerHtml'
    , createTextNode
    , createTextNode'
    , parentElement
    , getElementById
    , getElementById'
    , getElementsByClassName
    , getElementsByClassName'
    , addEventListener
    , addEventListener'
    , onClick
    , setStyleProperty
    , setStyleProperty'
    , (~>)
    ) where

import UHC.Ptr
import Util.String

data ElementPtr
type Element = Ptr ElementPtr

-- | Create an element with the specified name.
createElement :: String -> IO Element
createElement = __createElement . pack

createElement' :: PackedString -> IO Element
createElement' = __createElement 

-- | Set the inner HTML of an element. 
setInnerHtml :: Element -> String -> IO Element
setInnerHtml e html = _setInnerHtml (pack html) e 

setInnerHtml' :: Element -> PackedString -> IO Element
setInnerHtml' e html = _setInnerHtml html e 

_setInnerHtml :: PackedString -> Element -> IO Element
_setInnerHtml = __set $ pack "innerHTML"

-- | Writes HTML expressions or JavaScript code to a document.
documentWrite :: String -> IO ()
documentWrite = __documentWrite . pack 

documentWrite' :: PackedString -> IO ()
documentWrite' = __documentWrite

-- | Retrieve the document body element.
documentBody :: IO Element
documentBody = __body

-- | Retrieve the window element.
window :: IO Element
window = __window

-- | Add an element after the last child node of the specified element.
appendChild :: Element   -- ^ Parent node
            -> Element   -- ^ The child element
            -> IO ()
appendChild = __appendChild

(~>) :: Element -> Element -> IO ()
(~>) = flip __appendChild

-- | Add or set the attribute/value pair on the provided target element. 
setAttribute :: Element    -- ^ An element
             -> String     -- ^ Attribute name
             -> String     -- ^ Attribute value
             -> IO ()
setAttribute e n v = __setAttribute e (pack n) (pack v)

setAttribute' :: Element          -- ^ An element
              -> PackedString     -- ^ Attribute name
              -> PackedString     -- ^ Attribute value
              -> IO ()
setAttribute' = __setAttribute 

-- | Set the value attribute of an element (typically an input field).
setValue :: Element -> String -> IO ()
setValue e v = __set (pack "value") (pack v) e >> return ()

setValue' :: Element -> PackedString -> IO ()
setValue' e v = __set (pack "value") v e >> return ()

-- | Stringify an element.
toString :: Element -> IO String
toString e = __toString e >>= return . unpack 

toString' :: Element -> IO PackedString
toString' = __toString 

-- | Retrieve the HTML between the start and end tags of the object.
innerHtml :: Element -> IO String
innerHtml e = __innerHTML e >>= return . unpack 

innerHtml' :: Element -> IO PackedString
innerHtml' = __innerHTML 

createTextNode :: String -> IO Element
createTextNode = __createTextNode . pack

createTextNode' :: PackedString -> IO Element
createTextNode' = __createTextNode 

-- | Return the parent node of an element. This function returns a 
-- Maybe value, since a parent element doesn't always exist.
parentElement :: Element -> IO (Maybe Element)
parentElement e = __parentNode e >>= maybeElement 

-- | Return the element with the id attribute matching the specified value, if 
-- one exists. This function returns a Maybe Element to properly handle cases 
-- where no element is found.
getElementById :: String -> IO (Maybe Element)
getElementById e = (__getElementById $ pack e) >>= maybeElement

getElementById' :: PackedString -> IO (Maybe Element)
getElementById' e = __getElementById e >>= maybeElement

maybeElement :: Element -> IO (Maybe Element)
maybeElement e = return $ 
    if 0 == __boolean e
        then Nothing
        else Just e

data CollectionPtr 
type ElementCollection = Ptr CollectionPtr

-- | Return a list of elements which matches the provided class name(s).
getElementsByClassName :: String -> IO [Element]
getElementsByClassName = getElementsByClassName' . pack 

getElementsByClassName' :: PackedString -> IO [Element]
getElementsByClassName' cn = do
    ec <- __getElementsByClassName cn
    return [__itemAt ec (i - 1) | i <- [1 .. __length ec]]

-- | Add an event listener to the provided element.
addEventListener :: Element   -- ^ The target element
                 -> String    -- ^ Event type
                 -> IO ()     -- ^ Callback action
                 -> IO ()
addEventListener el = addEventListener' el . pack 

addEventListener' :: Element -> PackedString -> IO () -> IO ()
addEventListener' el v f = __wrap f >>= __addEventListener el v 

-- | Add an onClick event listener to the provided element.
onClick :: Element     -- ^ The target element
        -> IO ()       -- ^ Callback action
        -> IO ()
onClick el f = __wrap f >>= __onClick el 

data StylePtr
type StyleObject = Ptr StylePtr

-- | Set a property on an element's style object.
setStyleProperty :: Element -> String -> String -> IO ()
setStyleProperty e a v = setStyleProperty' e (pack a) (pack v)

setStyleProperty' :: Element -> PackedString -> PackedString -> IO ()
setStyleProperty' e a v = do
    style <- __style e
    __setStyleProperty style a v

foreign import js "document.createElement(%*)"
    __createElement :: PackedString -> IO Element

foreign import js "%1.appendChild(%2)"
    __appendChild :: Element -> Element -> IO ()

foreign import js "%1.setAttribute(%*)"
    __setAttribute :: Element -> PackedString -> PackedString -> IO ()

foreign import js "document.body"
    __body :: IO Element

foreign import js "window"
    __window :: IO Element

foreign import js "%1.toString"
    __toString :: Element -> IO PackedString

foreign import js "%1.innerHTML"
    __innerHTML :: Element -> IO PackedString

foreign import js "%1.parentNode"
    __parentNode :: Element -> IO Element

foreign import js "document.createTextNode(%*)"
    __createTextNode :: PackedString -> IO Element

foreign import js "document.getElementById(%*)"
    __getElementById :: PackedString -> IO Element

foreign import js "document.getElementsByClassName(%*)"
    __getElementsByClassName :: PackedString -> IO ElementCollection

foreign import js "%1.length"
    __length :: ElementCollection -> Int

foreign import js "%1[%2]"
    __itemAt :: ElementCollection -> Int -> Element

foreign import js "Boolean(%1)"
    __boolean :: a -> Int

foreign import prim "primSetAttr"
    __set :: PackedString -> a -> Ptr p -> IO (Ptr p)

foreign import js "%1.addEventListener(%2, %3, false)"
    __addEventListener :: Element -> PackedString -> FunPtr (IO ()) -> IO ()

foreign import js "%1.addEventListener('click', %2, false)"
    __onClick :: Element -> FunPtr (IO ()) -> IO ()

foreign import js "document.write(%*)"
    __documentWrite :: PackedString -> IO ()

foreign import js "wrapper"
    __wrap :: IO () -> IO (FunPtr (IO ()))

foreign import js "%1.setProperty(%2, %3)"
    __setStyleProperty :: StyleObject -> PackedString -> PackedString -> IO ()

foreign import js "%1.style"
    __style :: Element -> IO StyleObject

