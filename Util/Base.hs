module Util.Base 
    ( module Util.Router 
    , addScript
    , addScripts
    ) where

import Util.DOM
import Util.Router           ( wrap, onLoad, onReady )

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

