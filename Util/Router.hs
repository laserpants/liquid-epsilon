module Util.Router
    ( Route
    , str
    , num
    , atom
    , go
    , components
    , setLocationHash
    , setLocationHash'
    , locationHash
    , setMap
    , wrap
    , onLoad
    , onReady
    , onHashChange
    ) where

import Control.Monad               ( msum )
import UHC.Ptr
import Util.String

data RouteM a = RouteM { runR :: [String] -> (Maybe a, [String]) }

instance Monad RouteM where
    return a = RouteM $ \s -> (Just a, s)
    m >>= k = RouteM $ \s -> let (a, t) = runR m s
                           in  case a of
                                 Nothing -> (Nothing, s)
                                 Just a' -> runR (k a') t

type Route = RouteM (IO ())

nil :: forall a. RouteM a
nil = RouteM $ \_ -> (Nothing, [])

pop :: RouteM String
pop = RouteM $ f
  where f [] = (Nothing, [])
        f (a:as) = (Just a, as)

-- | Read a string segment.
str :: RouteM String
str = pop

-- | Read a numeric segment.
num :: RouteM Int
num = do 
    a <- pop
    case reads a of
      [(n, "")] -> return n
      _         -> nil

-- | Match a string literal.
atom :: String -> RouteM ()
atom e = do
    a <- pop
    if a == e then return () else nil

go :: IO () -> RouteM (IO ())
go g = RouteM $ f
  where f [] = (Just g, [])
        f _  = (Nothing, [])

runRoutes :: [String] -> [Route] -> IO ()
runRoutes s = maybe (return ()) id . msum . map f
  where f = fst . flip runR s

-- | Split a uri string into its path components.
components :: String -> [String]
components "" = []                                     
components ('#':xs) = components xs                    
components xs = f xs [] []                            
  where f :: String -> String -> [String] -> [String] 
        f [] ys zs = reverse $ map reverse $ g ys zs  
        f (x:xs) ys zs | x == '/'   = f xs [] $ g ys zs
                       | otherwise = f xs (x:ys) zs
        g :: String -> [String] -> [String]
        g a b | "" == a    = b
              | otherwise = (a:b)

-- | Set the anchor portion of the current URL.
setLocationHash :: String -> IO ()
setLocationHash = setLocationHash' . pack

setLocationHash' :: PackedString -> IO ()
setLocationHash' h = __set (pack "hash") h __location >> return ()

-- | Return a list of the slash-separated segments of the 
-- anchor portion of the current URL.
locationHash :: IO [String]
locationHash = __locationHash >>= return . components . unpack

-- | Set routing rules.
setMap :: [Route] -> IO ()
setMap r = do
    x <- wrap1 $ route r
    onHashChange x 
    -- This is to run the active route when the page loads.
    h <- locationHash
    runRoutes h r
  where route :: [Route] -> Ptr a -> IO b
        route rs e = do h <- locationHash
                        runRoutes h r
                        __preventDefault e
                        return __false

foreign import js "%1.preventDefault()"
    __preventDefault :: Ptr a -> IO ()

foreign import js "window.location.hash"
    __locationHash :: IO PackedString

foreign import prim "primSetAttr"
    __set :: PackedString -> a -> Ptr p -> IO (Ptr p)

foreign import js "location"
    __location :: a

foreign import js "wrapper"
    wrap :: IO a -> IO (FunPtr (IO a))

foreign import js "wrapper"
    wrap1 :: (Ptr a -> IO b) -> IO (FunPtr (Ptr a -> IO b))

foreign import js "window.addEventListener('load', %1, false)"
    onLoad :: FunPtr (IO a) -> IO ()

foreign import js "document.addEventListener('DOMContentLoaded', %1, false)"
    onReady :: FunPtr (IO a) -> IO ()

foreign import js "window.addEventListener('hashchange', %1, false)"
    onHashChange :: FunPtr (Ptr a -> IO b) -> IO ()

foreign import js "false"
    __false :: a

