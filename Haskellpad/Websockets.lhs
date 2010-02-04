% Haskellpad
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Haskellpad: Web Sockets Interface}
\maketitle

\section{Introduction}

This module defines an Abstract Data Type (ADT) called |Client| which models a single connection
over the new HTML5 standard `Web Sockets'. 

The Web Sockets protocol is essentially the same as ordinary TCP sockets, except the `same-domain'
restriction of web scripts is observed. Additional handshakes and frame protocols have been added.

The (draft) Websockets Protocol used in this module is defined here:

@http://tools.ietf.org/html/draft-hixie-thewebsocketprotocol-74@

> module Haskellpad.Websockets ( Client ()
>                              , acceptWeb
>                              , sendFrame
>                              , readFrame ) where

We will simply make use of the standard Haskell Network library to expose raw TCP sockets.

> import System.IO
> import System.IO.Error
> import Data.Char (ord, chr)
> import Network (accept, Socket)

\section{Abstract Data Type}

Internally, the Client data type maps directly to a connection handle and is interacted using
the standard Haskell IO functions.

> data Client = Client Handle deriving (Show, Eq)

The (exposed) constructor of the Client type is |acceptWeb|, that waits for a connection to
the specified socket and initializes the handle. This thread-blocks until a client connects.

It also makes a server handshake to initiate Web Sockets communication.

> acceptWeb :: Socket -> IO Client 
> acceptWeb socket = do (h,location,port) <- accept socket
>                       hPutStr h (serverHandshake location port)
>                       hSetBuffering h NoBuffering
>                       return $ Client h
>  where serverHandshake location port = concat $ 
>         [ "HTTP/1.1 101 Web Socket Protocol Handshake\r\n"
>         , "Upgrade: WebSocket\r\n"
>         , "Connection: Upgrade\r\n"
>         , "WebSocket-Origin: http://" ++ location ++ "\r\n"
>         , "WebSocket-Location: ws://" ++ location ++ ":" ++ show port ++ "/\r\n"
>         , "WebSocket-Protocol: sample\r\n\r\n\0"
>         ]

\section{Message Frames}

The Web Sockets protocol specifies that each message `frame' should start with a null character, 
|'\0'| or |chr 0|, and end with its complement, |'\FF'| or |chr 255|. Hence the outgoing transmission
interface(|sendFrame|) performs this wrapping.

If the client is disconnected or disconnects in the process of sending, then we return |False|
to indicate transmission failed.

\emph{Note:} Usually if a transmission fails, the transmission will continue to fail.

> sendFrame :: Client -> String -> IO Bool
> sendFrame (Client h) s = do
>   v <- try $ do
>     hPutChar h (chr 0)
>     hPutStr h s
>     hPutChar h (chr 255)
>   case v of Left _  -> return False
>             Right _ -> return True
> 

The |readFrame| function reads a single message frame from the |Client|, which is wrapped in the same 
way as server frames.

If the connection fails at any point while reading the frame, it will return a |Left| value with
the string read so far. Otherwise, it returns a |Right| value with the full string.

This will thread-block while waiting for input.

> readFrame :: Client -> IO (Either String String)
> readFrame (Client h) = readFrame' h ""
>    where readFrame' h str = do
>           new' <- try $ hGetChar h
>           case new' of 
>              Left _    -> return $ Left str
>              Right new -> case ord new of 
>                              0   -> readFrame' h "" 
>                              255 -> return $ Right str
>                              _   -> readFrame' h (str++[new]) 

\end{document}
