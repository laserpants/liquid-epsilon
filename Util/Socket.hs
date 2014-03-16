module Util.Socket 
    ( Socket
    , close
    , connectAnd
    , newSocket
    , newSocket'
    , onClose
    , onError
    , onMessage
    , onOpen
    , send
    , send'
    ) where

import UHC.Ptr
import Util.Base
import Util.DOM 
import Util.String

data SocketPtr
type Socket = Ptr SocketPtr

-- | Creates a web socket or returns Nothing if the connection attempt fails.
newSocket :: String -> IO (Maybe Socket)
newSocket p = __init >> (_socket . pack $ p)

newSocket' :: PackedString -> IO (Maybe Socket)
newSocket' p = __init >> _socket p

__init :: IO ()
__init = do
    def <- __defined __windowNewSocket
    case def of
        0 -> do body <- documentBody
                s <- createElement "script"
                setAttribute s "type" "text/javascript"
                setInnerHtml s "window.__newSocket = function(path) { try { return 'MozWebSocket' in window ? new MozWebSocket(path) : new WebSocket(path) } catch(e) { console.log(e); return undefined; }}"
                s ~> body
        _ -> return ()

_socket :: PackedString -> IO (Maybe Socket)
_socket path = do
    s <- __socket path
    def <- __defined s
    return $ case def of
               0 -> Nothing
               _ -> Just s

onMessage :: Socket -> (String -> IO ()) -> IO ()
onMessage sock f = do
    f' <- __wrap1 $ f . unpack . __eventData 
    __set (pack "onmessage") f' sock >> return ()

onOpen :: Socket -> IO () -> IO ()
onOpen = socketEvent "onopen"

onError :: Socket -> IO () -> IO ()
onError = socketEvent "onerror"

onClose :: Socket -> (String -> IO ()) -> IO ()
onClose sock f = do
    f' <- __wrap1 $ f . unpack . __eventReason
    __set (pack "onclose") f' sock >> return ()

socketEvent :: String -> Socket -> IO () -> IO ()
socketEvent event sock f = do
    f' <- wrap f
    __set (pack event) f' sock >> return ()

-- | Send a message on a socket.
send :: Socket -> String -> IO ()
send s = __send s . pack

send' :: Socket -> PackedString -> IO ()
send' = __send

-- | Close a socket.
close :: Socket -> IO ()
close = __close

connectAnd :: String            -- ^ URI
           -> (Socket -> IO ())  -- ^ Callback with socket handle as argument
           -> IO ()
connectAnd path go = newSocket path 
                  >>= maybe (return ()) id . fmap go 

foreign import js "%1.data"
    __eventData :: a -> PackedString

foreign import js "%1.reason"
    __eventReason :: a -> PackedString

foreign import prim "primSetAttr"
    __set :: PackedString -> a -> Ptr p -> IO (Ptr p)

foreign import js "window.__newSocket(%*)"
    __socket :: PackedString -> IO Socket

foreign import js "window.__newSocket"
    __windowNewSocket :: a

foreign import js "Boolean(%*)"
    __defined :: a -> IO Int

foreign import js "%1.send(%2)"
    __send :: Socket -> PackedString -> IO ()

foreign import js "%1.close()"
    __close :: Socket -> IO ()

foreign import js "wrapper"
    __wrap1 :: (String -> IO ()) -> IO (FunPtr (String -> IO ()))

