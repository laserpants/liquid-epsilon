module Util.HTML 
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

import Data.Monoid

data HtmlM a
    = Parent   !String      -- ^ Tag
               !String      -- ^ Opening tag
               !String 
               !String      -- ^ Closing tag
               !(HtmlM a)   -- ^ Child element
    | Leaf     !String      -- ^ Tag
               !String      -- ^ Opening tag
               !String      -- ^ Closing tag
    | Content  !String      -- ^ HTML content
    | forall b c. 
      Append   (HtmlM b)    -- ^ Concatenation of two
               (HtmlM c)    -- ^ HTML elements
    | Attr     !String      -- ^ Raw key
               !String      -- ^ Attribute key
               !String      -- ^ Attribute value
               !(HtmlM a)   -- ^ Target element
    | Empty                 -- ^ Empty element

type Html = HtmlM ()

data Attribute = Attribute !String !String !String

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

renderHtml :: HtmlM t -> String
renderHtml = go ""
  where go :: String -> HtmlM t  -> String 
        go s (Parent _ open r close content)
          = open ++ s ++ r ++ renderHtml content ++ close
        go s (Leaf _ open close) 
          = open ++ s ++ close
        go s (Content content) = escapeHtmlEntities content
        go s (Append h1 h2) = go s h1 ++ go s h2
        go s (Attr _ key value h) 
          = flip go h 
          $ key 
          ++ escapeHtmlEntities value 
          ++ "\"" 
          ++ s
        go _ Empty = ""

escapeHtmlEntities :: String -> String
escapeHtmlEntities "" = "" 
escapeHtmlEntities (c:cs) =
    let c' = case c of
               '<'  -> "&lt;" 
               '>'  -> "&gt;"
               '&'  -> "&amp;"
               '"'  -> "&quot;"
               '\'' -> "&#39;"
               x    -> [x]
    in c' ++ escapeHtmlEntities cs

attribute :: String -> String -> String -> Attribute
attribute = Attribute

empty :: Html
empty = Empty

parent :: String -> String -> String -> String -> Html -> Html
parent = Parent

leaf :: String -> String -> String -> Html
leaf = Leaf

toHtml :: String -> HtmlM a
toHtml = Content

makeAttr :: String -> String -> Attribute
makeAttr a = Attribute a (" " ++ a ++ "=\"")

makePar :: String -> Html -> Html
makePar h = Parent h ("<" ++ h) ">" ("</" ++ h ++ ">")

makeLeaf :: String -> Html
makeLeaf a = Leaf a ("<" ++ a) ">"

-- Shorthand infix operator

($<) :: (Html -> Html) -> String -> Html
($<) a = a . toHtml

