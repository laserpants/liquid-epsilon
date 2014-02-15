module Util.HTML.Handlebars where

import Util.HTML
import Util.Handlebars
import Util.JSON              ( ToJSON )

handlebar :: String -> Html
handlebar s = leaf "" ("{{" ++ s) "}}"

each :: String -> Html -> Html
each i = parent "" ("{{#each " ++ i) "}}" "{{/each}}"

-- Shorthand infix operator

($?) :: (Html -> Html) -> String -> Html
($?) a = a . handlebar

renderWithHtml :: (ToJSON a) => Html -> a -> IO String
renderWithHtml htm obj = do
    f <- compile $ renderHtml htm
    h <- render f obj
    return h

