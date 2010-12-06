% Opardum
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Web Sockets Interface}
\ignore{

> {-# OPTIONS_GHC -fno-warn-name-shadowing #-}
> {-# LANGUAGE OverloadedStrings, ViewPatterns, FlexibleInstances, DeriveDataTypeable #-}

}
\maketitle

\section{Introduction}

This module defines an Abstract Data Type (ADT) called |Client| which models a single connection
over the new HTML5 standard `Web Sockets'. 

The Web Sockets protocol is essentially the same as ordinary TCP sockets, except the `same-domain'
restriction of web scripts is observed. Additional handshakes and frame protocols have been added.

The (draft) Websockets Protocol used in this module is defined here:

@http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-76@

This is the version used by the latest websockets-enabled browsers. Compatibility is also included
for the previous version of the protocol, documented here:

@http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-75@
                                                                
>
> module Opardum.Websockets ( Client () 
>                           , acceptWeb
>                           , sendFrame
>                           , readFrame
>                           , disconnect ) where
> 

We will simply make use of the standard Haskell @Network@ library to expose raw TCP sockets.

> import Network
> import System.IO
> import System.IO.Error hiding (catch)

We will also import @Concurrent@ libraries, for use with handshake threads.

>-- import Control.Concurrent 
> import Data.Char (ord, chr, isDigit, isSpace)

Finally we also import @ByteString@ and the @Builder@s from the @cereal@ package, in order
to parse the headers of the WebSocket keys, and the @MD5@ module to perform the MD5 hash
required by the protocol.

> import qualified Data.ByteString.Char8 as B
> import Data.ByteString.Char8(append,ByteString) 
> import qualified Data.Serialize.Builder as C
> import qualified Opardum.MD5 as MD5
> import Data.Word

And some other miscellaneous imports:

> import Data.Maybe(fromJust)
> import Control.Monad
> import Control.Monad.Instances
> import Control.Monad.Trans
> import Control.Concurrent.Forkable
> 

\section {Abstract Data Type}

We model a |Client| as a wrapper around a connection handle, and a
unique integer to identify it.

> data Client = Client Integer Handle deriving (Show, Eq)

We need to define an |Ord| instance in order to be able to store these
in a set. 

> instance Ord Client where
>    compare (Client i1 a) (Client i2 b) = i1 `compare` i2

|acceptWeb| is the only exposed way to produce a |Client|. It is made
a generic |MonadIO| (with |ForkableMonad| for threads) so that it can be
used in Processes or any other IO monad without depending on the |Processes|
module. This keeps this websockets implementation fairly project-agnostic.

The function listens on a provided socket, performs the websocket handshake
with each incoming connection on a new thread, and runs the specified action.

It does not close the connection when the action completes. It must be 
explicitly closed via |disconnect|.

> acceptWeb :: (MonadIO m, ForkableMonad m) => Socket -> (Client -> m a) -> m ()
> acceptWeb socket f = 
>  let me count = do 
>        (h,_,_) <- liftIO $ accept socket
>        forkIO $ do v <- liftIO $ handshakeExchange h
>                    liftIO $ hSetBuffering h NoBuffering
>                    let client = Client count h
>                    when v $ (f client >> return () ) 
>        me (count + 1)
>  in me 0

|disconnect| simply closes a connection with the client.

> disconnect :: Client -> IO ()
> disconnect (Client i h) = hClose h

\section{Message Frames}

The Web Sockets protocol specifies that each message `frame' should start with a null character, 
|'\0'| or |chr 0|, and end with its complement, |'\FF'| or |chr 255|. Hence the outgoing transmission
interface(|sendFrame|) performs this wrapping.

If the client is disconnected or disconnects in the process of sending, then we return |False|
to indicate transmission failed.

\emph{Note:} Usually if a transmission fails, the transmission will continue to fail.

> sendFrame :: Client -> String -> IO Bool
> sendFrame (Client _ h) s = do
>     v <- try $ do hPutChar h (chr 0)
>                   hPutStr h s
>                   hPutChar h (chr 255)
>     case v of Left _  -> do putStrLn $ "dead";  return False
>               Right _ -> return True

> instance Monad (Either String) where
>   return v = Right v
>   fail s = Left s
>   (Left s) >>= _ = Left s
>   (Right v) >>= f = f v
> 
> 

The |readFrame| implementation reads in a frame according to the algorithm specified in the protocol
documentation.
 
> readFrame :: Client -> IO (Either String String)
> readFrame c@(Client _ h) = do
>     new' <- try $ hGetChar h
>     case new' of
>       Left _ -> return $ Left ""
>       Right new -> case ord new of 
>                       0 -> readFrame' h ""
>                       _ -> disconnect c >> return (Left "")
>    where readFrame' h str = do
>            new' <- try $ hGetChar h
>            case new' of 
>               Left _    -> return $ Left str
>               Right new -> case ord new of 
>                               0   -> readFrame' h "" 
>                               255 -> return $ Right $ str
>                               _   -> readFrame' h (str++[new]) 
>

The Handshake is implemented such that version 75 and 76 of the protocol are both supported.

> handshakeExchange :: Handle -> IO Bool
> handshakeExchange h = do v <- grabHeaders h
>                          case v of
>                            Left v -> try (hPutStrLn h v) >> hClose h >> return False
>                            Right v -> sendResponse h v >> return True
> 
> sendResponse h v = B.hPutStr h (response v)
> response v = let origin = fromJust $ lookup Origin v
>                  location = "ws://" `append` fromJust (lookup Host v) `append` fromJust (lookup Resource v)
>                  key1 = fromJust $ lookup WebSocketV76Key1 v
>                  key2 = fromJust $ lookup WebSocketV76Key2 v
>                  keydata = fromJust $ lookup WebSocketV76KeyData v
>                in if (fromJust $ lookup WebSocketVersion v) == "hixie-76"
>                        then let digest = let [a,b] = map digestProcess [key1, key2] 
>                                              conc = a `append` b `append` keydata
>                                                 in MD5.md5rawsum (conc)
>                                 digestProcess x = case B.readInt (B.filter (isDigit) x) 
>                                                     of Just (i,_) -> convertToBE ((fromIntegral i :: Word32) `div` ensuregt0 (fromIntegral $ B.count ' ' x)) 
>                                                        Nothing -> error "Let's hope this doesn't happen" 
>                                 convertToBE i = C.toByteString (C.putWord32be (fromIntegral i))
>                                 ensuregt0 0 = 1 
>                                 ensuregt0 v = v
>                              in serverHandshake76 origin location digest
>                        else serverHandshake75 origin location
>     where
>         serverHandshake75 origin location = B.concat $ 
>             [ "HTTP/1.1 101 Web Socket Protocol Handshake\r\n"
>             , "Upgrade: WebSocket\r\n"
>             , "Connection: Upgrade\r\n"
>             , "WebSocket-Origin: " `append` origin `append` "\r\n"
>             , "WebSocket-Location: " `append` location `append` "\r\n"
>             , "WebSocket-Protocol: sample\r\n\r\n\0"
>             ]
>         serverHandshake76 origin location digest= B.concat $ 
>             [ "HTTP/1.1 101 WebSocket Protocol Handshake\r\n"
>             , "Upgrade: WebSocket\r\n"
>             , "Connection: Upgrade\r\n"
>             , "Sec-WebSocket-Origin: " `append` origin `append` "\r\n"
>             , "Sec-WebSocket-Location: " `append` location `append` "\r\n"
>             , "Sec-WebSocket-Protocol: sample\r\n\r\n" `append` digest `append` "\r\n\0"
>             ]
>  
> 
> data HeaderContent = Host
>                    | Resource
>                    | WebSocketV76Key1
>                    | WebSocketV76Key2
>                    | Connection
>                    | WebSocketV76KeyData
>                    | WebSocketProtocol
>                    | Origin
>                    | WebSocketVersion
>                    | Upgrade  
>                    deriving (Show,Eq)
> 
> trim x = case B.span isSpace x of (_,v) -> fst (B.spanEnd isSpace v)
> parseHeaders :: [ByteString] -> (Either String [(HeaderContent,ByteString)])
> parseHeaders x = case map (B.filter (/='\r')) x of
>                    x:xs -> do x <- parseHead x
>                               y <- sequence (map parseBlock xs)                               
>                               checkValidWS (x:y)
>    where parseHead (B.splitAt 4 -> (x,y)) = if (x /= "GET ") then Left "Invalid HTTP request"
>                                              else Right (Resource, trim (B.take (B.length y - 8) y))
>          parseBlock (B.break (==':') -> (x,y)) = do v <- parseContent x                                                    
>                                                     return (v, trim (B.drop 2 y))
>          parseContent "Host" = Right Host
>          parseContent "Sec-WebSocket-Key1" = Right WebSocketV76Key1
>          parseContent "Sec-WebSocket-Key2" = Right WebSocketV76Key2
>          parseContent "Upgrade" = Right Upgrade
>          parseContent "Origin" = Right Origin
>          parseContent "Sec-WebSocket-Protocol" = Right WebSocketProtocol
>          parseContent "WebSocket-Protocol" = Right WebSocketProtocol
>          parseContent "Connection" = Right Connection
>          parseContent q = Left $ "Invalid Header" ++ B.unpack q
>          checkValidWS stuff = let stuff' = map fst stuff in
>                                 if all (`elem` stuff') [Host,Resource,Origin,Upgrade,Connection]
>                                 then if (WebSocketV76Key1 `elem` stuff') then Right ((WebSocketVersion,"hixie-76"):stuff)
>                                                                          else Right ((WebSocketVersion,"hixie-75"):stuff)
>                                 else Left $ "Invalid Websocket Header!" ++ show stuff
> 
> 
> grabHeaders h = do ev <- grabHeaders' h [] 0 
>                    putStrLn "Got basic headers" 
>                    case ev of 
>                      Right v -> if fromJust (lookup WebSocketVersion v) == "hixie-76" then do
>                                     x <- try $ fmap (B.filter (/='\r')) $ B.hGet h 8
>                                     case x of Right x' -> return $ Right $ (WebSocketV76KeyData,x'):v
>                                               _        -> return $ Left $ "Failed to Read WebSocket data key!"                                                           
>                                   else return ev
>                      v@(Left _) -> return ev
> 
> grabHeaders' :: Handle -> [ByteString] -> Int -> IO (Either String [(HeaderContent, ByteString)])
> grabHeaders' h accum len = do v <- try $ hGetLine h
>                               if (len > 12) then return $ Left "Request too long!" 
>                                 else case v of
>                                      Right v' -> if (filter (/='\r') v') == "" 
>                                                    then return $ parseHeaders (reverse accum)
>                                                    else grabHeaders' h  ((B.pack v'):accum) (len+1)                              
>                                      Left v' -> return $ Left $ show v'
>

\end{document}
