module Util.HTML.PS 
    ( Html
    , Attribute
    , Attributable
    , renderHtml
    , toHtml
    , attribute
    , empty
    , parent
    , leaf
    , makeAttr
    , makePar
    , makeLeaf
    , (!)
    , ($<)
    ) where

import Data.Monoid                ( Monoid, mappend )
import System.IO.Unsafe
import Util.DOM

data HtmlM a
    = Parent   !PackedString    -- ^ Tag
               !PackedString    -- ^ Opening tag
               !PackedString 
               !PackedString    -- ^ Closing tag
               !(HtmlM a)       -- ^ Child element
    | Leaf     !PackedString    -- ^ Tag
               !PackedString    -- ^ Opening tag
               !PackedString    -- ^ Closing tag
    | Content  !PackedString    -- ^ HTML content
    | forall b c. 
      Append   (HtmlM b)        -- ^ Concatenation of two
               (HtmlM c)        -- ^ HTML elements
    | Attr     !PackedString    -- ^ Raw key
               !PackedString    -- ^ Attribute key
               !PackedString    -- ^ Attribute value
               !(HtmlM a)       -- ^ Target element
    | Empty                     -- ^ Empty element

type Html = HtmlM ()

data Attribute = Attribute !PackedString !PackedString !PackedString

instance Monoid a => Monoid (HtmlM a) where
    mempty  = Empty
    mappend = Append 
    mconcat = foldr Append Empty

instance Monad HtmlM where
    return _ = Empty
    (>>) = Append 
    h1 >>= f = h1 >> f (error "")

class Attributable h where
    (!) :: h -> Attribute -> h

instance Attributable (HtmlM a) where
    (!) h (Attribute raw key val) = Attr raw key val h

instance Attributable (HtmlM a -> HtmlM b) where
    (!) f (Attribute raw key val) = Attr raw key val . f 

renderHtml :: HtmlM t -> PackedString
renderHtml = go __emptyStr
  where go :: PackedString -> HtmlM t -> PackedString
        go s (Parent _ open r close content)
          = __concat5 open s r (renderHtml content) close
        go s (Leaf _ open close)
          = __concat3 open s close
        go s (Content content) = escapeHtmlEntities content
        go s (Append h1 h2) = __concat (go s h1) (go s h2)
        go s (Attr _ key value h)
          = flip go h 
          $ __concat4 key (escapeHtmlEntities value) __q s
        go _ Empty = __emptyStr

foreign import js "%1.concat(%2)"
    __concat :: PackedString -> PackedString -> PackedString

foreign import js "%1.concat(%2).concat(%3)"
    __concat3 :: PackedString -> PackedString -> PackedString -> PackedString

foreign import js "%1.concat(%2).concat(%3).concat(%4)"
    __concat4 :: PackedString -> PackedString -> PackedString -> PackedString -> PackedString

foreign import js "%1.concat(%2).concat(%3).concat(%4).concat(%5)"
    __concat5 :: PackedString -> PackedString -> PackedString -> PackedString -> PackedString -> PackedString

foreign import js "String()"
    __emptyStr :: PackedString

foreign import js "String('\"')"
    __q :: PackedString

escapeHtmlEntities :: PackedString -> PackedString
escapeHtmlEntities = let a = unsafePerformIO __init in seq a . __escapeHtml 

foreign import prim "escapeHtml"
    __escapeHtml :: PackedString -> PackedString

__init :: IO ()
__init = do
    def <- __defined 
    case def of
        0 -> do body <- documentBody
                s <- createElement "script"
                setAttribute s "type" "text/javascript"
                setInnerHtml s "\
\var entityMap = {\
\  '&':'&amp;',\
\  '<':'&lt;',\
\  '>':'&gt;',\
\  '\"':'&quot;',\
\  \"'\":'&#39;',\
\  '/':'&#x2F;'\
\};\
\function escapeHtml(string) {\
\  return String(string).replace(/[&<>\"'\\/]/g, function (s) {\
\    return entityMap[s];\
\  });\
\}"
                s ~> body
        _ -> return ()

foreign import js "window.hasOwnProperty('escapeHtml')"
    __defined :: IO Int

attribute :: PackedString -> PackedString -> PackedString -> Attribute
attribute = Attribute

empty :: Html
empty = Empty

parent :: PackedString -> PackedString -> PackedString -> PackedString -> Html -> Html
parent = Parent

leaf :: PackedString -> PackedString -> PackedString -> Html
leaf = Leaf

toHtml :: PackedString -> HtmlM a
toHtml = Content

makeAttr :: PackedString -> PackedString -> Attribute
makeAttr a = Attribute a $ __makeAttr a 

foreign import js "' '.concat(%1).concat('=\"')"
    __makeAttr :: PackedString -> PackedString

makePar :: PackedString -> Html -> Html
makePar h = Parent h (__makeLeft h) __gt (__makeClose h)

foreign import js "'</'.concat(%1).concat('>')"
    __makeClose :: PackedString -> PackedString

makeLeaf :: PackedString -> Html
makeLeaf a = Leaf a (__makeLeft a) __gt

foreign import js "'<'.concat(%1)"
    __makeLeft :: PackedString -> PackedString

foreign import js "String('>')"
    __gt :: PackedString

-- Shorthand infix operator

($<) :: (Html -> Html) -> PackedString -> Html
($<) a = a . toHtml

