% Opardum
% A Collaborative Code Editor
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Client Registry }
\maketitle

\section{Introduction}

In order to create a barrier of abstraction between broader, per-document threads and specific,
per-client threads, we create here an Abstract Data Type (ADT) for use in the @ConcurrencyControl@
module that manages client threads so that the broader document threads need not deal with the
mechanics of creating, removing, and communicating to a client thread.

> module Opardum.ClientRegistry
>   ( ClientRegistry ()
>   , createRegistry
>   , createClient
>   , removeClient
>   , sendOp
>   , sendSnapshot
>   , numClients
>   ) where

> import Opardum.ClientThreads
> import Opardum.Processes
> import Opardum.Websockets
> import Opardum.OperationalTransforms
> import Opardum.DocumentManager.Types
>
> import Control.Applicative((<$>))
> import qualified Data.Map as M
> import qualified Data.Traversable as T
> import Data.IORef

\section{Implementation}

\subsection{Data types}

Seeing as the |ClientRegistry| is intended only to be used in an IO context, we will model the
registry as a newtype around an |IORef| that points to the actual data used, somewhat like the
common C idiom.

> newtype ClientRegistry = ClientRegistry (IORef (ClientRegistryData))

Each client has two threads, a listener, which listens for incoming packets from the client,
and a shouter, which broadcasts operations from the server to the client. The |ClientRegistry|
is responsible for managing outgoing communication to these threads, and so a communication
channel to the shouter is stored in the |ClientRegistryData|. No channel is made for the listener,
seeing as the listener waits on client input and does not need server input except for when it is
spawned. For more information about these threads, see the @ClientThreads@ module.

A transmission |Packet| includes, in addition to the |Op|, the number of operations received from the client
by the server since the server last sent ops (the $d_s$ value mentioned in the @ConcurrencyControl@
module). This value is also managed by the |ClientRegistry| and is attached to an |Op| to form a |Packet|
before it is transmitted.

Furthermore, the |ClientRegistry| must be aware of the |DocumentManager| which created it, so that
clients can direct new operations to the |DocumentManager|.

With that in mind, we define the |ClientRegistryData| structure as a pair, containing the following:
\begin{itemize}
\item A @Map@ of |Client|s to a |ClientData| record: a channel to the shouter, and the $d_s$ value.
\item A channel for the |DocumentManager| which created it.
\end{itemize}

> type ClientRegistryData = (M.Map Client ClientData, Chan DocumentManagerMsg)
> data ClientData = ClientData { getShouter  :: ChanFor Shouter
>                              , d_value     :: Int
>                              }

\subsection {Functions}

The constructor for this ADT, |createRegistry|, is simply a wrapper around an empty map:

> createRegistry :: Chan DocumentManagerMsg -> IO ClientRegistry
> createRegistry toDM = ClientRegistry <$> newIORef (M.empty, toDM)

To create a client, we simply spawn the two client threads, and set the $d_s$ value to an
obvious amount, say, 0.

> createClient :: ClientRegistry -> Client -> IO ()
> createClient (ClientRegistry reg) c = do
>   (cd, toDM) <- readIORef reg
>   shouter  <- runProcess' Shouter (c, toDM)
>   runProcess' Listener (c, toDM)
>   writeIORef reg (M.insert c (ClientData shouter 0) cd, toDM)

Similarly, the |removeClient| function simply sends @Terminate@ messages to the client threads,
and then removes the |ClientData| from the registry.

> removeClient :: ClientRegistry -> Client -> IO ()
> removeClient (ClientRegistry reg) c = do
>   (cd, toDM) <- readIORef reg
>   let (ClientData shouter _) = cd M.! c
>   closeClient c
>   STerminate ~> shouter
>   writeIORef reg (M.delete c cd, toDM)

The |sendSnapshot| function sends a single op (the composed document snapshot) to the client.

> sendSnapshot :: ClientRegistry -> Op -> Client -> IO ()
> sendSnapshot (ClientRegistry reg) op c = do
>   (cd, toDM) <- readIORef reg
>   let (ClientData shouter _) = cd M.! c
>   Shout (0,op) ~> shouter

The |sendOp| function is slightly more involved. Here we are required to send operations (with
correct $d_s$ numbers), to every client \emph{except} the one from which the operation originated.
We also have to increase the $d_s$ number for that client so that it can transform future
operations we send to it correctly.

> sendOp :: ClientRegistry -> Op -> Client -> IO ()
> sendOp (ClientRegistry reg) op c = do
>   (cd, toDM) <- readIORef reg
>   let (ClientData s d) = cd M.! c
>   cd' <- flip T.mapM (M.delete c cd) $ \(ClientData shouter d) -> do
>          Shout (d, op) ~> shouter
>          return $ ClientData shouter 0
>   writeIORef reg (M.insert c (ClientData s (d+1)) cd', toDM)

Finally, the |numClients| function is simply a wrapper around the |size| function in @Data.Map@.

> numClients :: ClientRegistry -> IO Int
> numClients (ClientRegistry reg) =  readIORef reg >>= \(cd, _) -> return $ M.size cd

\end{document}

