module Util.Storage
    ( set
    , set'
    , get
    , get'
    , deleteKey
    , deleteKey'
    , index
    , index'
    , flush
    , setTTL
    , setTTL'
    , getTTL
    , getTTL'
    , storageSize
    , currentBackend
    , reInit
    , storageAvailable
    ) where

import Util.JSON
import Util.String
import UHC.Ptr

-- | Saves a value to local storage.
set :: (ToJSON a) => String -> a -> IO ()
set = set' . pack 

set' :: (ToJSON a) => PackedString -> a -> IO ()
set' key val = __set key (pack $ valueToStr $ toJSON val)

-- | Retrieves the stored value matching the given key,
-- if one exists.
get :: (FromJSON a) => String -> IO (Maybe a)
get = get' . pack

get' :: (FromJSON a) => PackedString -> IO (Maybe a)
get' key = do
    x <- __get key
    return $ if 0 == __boolean x
        then Nothing
        else case (parse json $ unpack x) of
               []       -> Nothing
               [(a, _)] -> fromJSON a

-- | Removes a key from the storage.
deleteKey :: String -> IO ()
deleteKey = __delete . pack

deleteKey' :: PackedString -> IO ()
deleteKey' = __delete 

-- | Returns all keys currently in use as a list.
index :: IO [String]
index = index' >>= return . map unpack

index' :: IO [PackedString]
index' = do
    i <- __index
    return $ case __indexLength i of
        0 -> []
        n -> let g = __indexElement i 
            in  [g x | x <- [0 .. n - 1]]

-- | Clears the cache.
flush :: IO ()
flush = __flush

-- | Sets a TTL (in milliseconds) for an existing key. 
-- Use 0 or negative value to clear TTL.
setTTL :: String -> Int -> IO ()
setTTL = __setTTL . pack 

setTTL' :: PackedString -> Int -> IO ()
setTTL' = __setTTL

-- | Gets remaining TTL (in milliseconds) for a key or 0 if not TTL 
-- has been set.
getTTL :: String -> IO Int
getTTL = __getTTL . pack 

getTTL' :: PackedString -> IO Int
getTTL' = __getTTL 

-- | Returns the size of the stored data in bytes.
storageSize :: IO Int
storageSize = __storageSize

-- | Returns the storage engine currently in use or 'false' if none.
currentBackend :: IO String
currentBackend = __currentBackend >>= return . unpack

-- | Reloads the data from browser storage.
reInit :: IO ()
reInit = __reInit

-- | Returns True if storage is available.
storageAvailable :: IO Bool
storageAvailable = do
    b <- __storageAvailable
    return $ b /= 0

foreign import js "$.jStorage.set(%*)"
    __set :: PackedString -> PackedString -> IO ()

foreign import js "$.jStorage.get(%*)"
    __get :: PackedString -> IO PackedString

foreign import js "$.jStorage.deleteKey(%*)"
    __delete :: PackedString -> IO ()

data IndexPtr
type Index = Ptr IndexPtr

foreign import js "$.jStorage.index()"
    __index :: IO Index

foreign import js "%1[%2]"
    __indexElement :: Index -> Int -> PackedString

foreign import js "%1.length"
    __indexLength :: Index -> Int

foreign import js "$.jStorage.flush()"
    __flush :: IO ()

foreign import js "$.jStorage.setTTL(%*)"
    __setTTL :: PackedString -> Int -> IO ()

foreign import js "$.jStorage.getTTL(%*)"
    __getTTL :: PackedString -> IO Int

foreign import js "$.jStorage.storageSize()"
    __storageSize :: IO Int

foreign import js "$.jStorage.currentBackend()"
    __currentBackend :: IO PackedString

foreign import js "$.jStorage.reInit()"
    __reInit :: IO ()

foreign import js "$.jStorage.storageAvailable()"
    __storageAvailable :: IO Int

foreign import js "Boolean(%*)"
    __boolean :: a -> Int

