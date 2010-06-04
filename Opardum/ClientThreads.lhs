% Opardum
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Client Threads}
\maketitle

\ignore{

> {-# LANGUAGE TypeFamilies #-}

}
\section{Introduction}

In this module we define two threads, which are responsible for communicating with clients through
the @Websockets@ module's |Client| ADT. They also perform the serialization required to transmit
operations over the wire. These two threads are called the |listener| and the |shouter|. One of each
exists for each connected client.

> module Opardum.ClientThreads where
>
> import Opardum.DocumentManager.Types
> import Opardum.ConcurrencyControl (Packet)
> import Opardum.Transport
> import Opardum.Websockets
> import Opardum.Processes

\section{Implementation}

\subsection{Listener}

The |listener| monitors the client, deserializes operations that come in, and then sends them to 
the |documentManager| thread defined in the @ConcurrencyControl@ module. It has no message type
as it waits on client input not application input.

If the client input somehow breaks, for example if the client disconnects, the listener signals
the document manager that the client should be removed.

> data Listener = Listener
> instance Process Listener where
>   type ProcessCommands Listener = ()
>   type ProcessState Listener = ()
>   type ProcessInfo Listener = (Client, Chan DocumentManagerMsg)
>   continue _ = do
>      (client, toDM) <- getInfo
>      read <- io $ readFrame client 
>      debug $ show read
>      case read of
>        Left _    -> RemoveClient client ~> toDM 
>        Right str -> case deserialize str of
>                       Nothing -> RemoveClient client ~> toDM
>                       Just p  -> do 
>                         debug $ "Recieved " ++ show p
>                         NewOp client p ~> toDM 
>                         continue Listener
>   nullChannel _ = True

\subsection{Shouter}

The |shouter| listens on a channel from the @ClientRegistry@, which broadcasts operations to it 
for serialization and transmission to the client.

> data ShouterMsg = STerminate
>                 | Shout Packet

If the packet fails to send, for example if the client disconnects, the shouter signals the 
document manager that the client should be removed.

> data Shouter = Shouter
> instance Process Shouter where
>   type ProcessCommands Shouter = ShouterMsg
>   type ProcessState Shouter = ()
>   type ProcessInfo Shouter = (Client, Chan DocumentManagerMsg)
>   continue _ = do
>     (client, toDM) <- getInfo
>     inbox <- getInbox
>     msg <- grabMessage inbox
>     case msg of
>        STerminate -> return ()
>        Shout pack -> do v <- io $ sendFrame client (serialize pack) 
>                         debug $ "shouted to " ++ show client ++ ": " ++ serialize pack
>                         if v then continue Shouter
>                              else RemoveClient client ~> toDM


\end{document}
