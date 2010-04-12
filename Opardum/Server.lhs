% Opardum
% A Collaborative Code Editor
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Server}
\maketitle

\section{Introduction}

In this module are contained singleton threads that manage initial client connections, as well
as functions that set up the TCP sockets for client communication.

\section{Implementation}

> module Opardum.Server
>   ( clientManager
>   , ClientManagerMsg (..)
>   , portListener
>   , server
>   ) where
>
> import Opardum.Websockets
> import Opardum.ConcurrentChannels
> import Opardum.ConcurrencyControl
> import Opardum.Storage
>
> import qualified Data.Map as M
> import Control.Monad.State
> import Network

\subsection{Client Manager}

The client manager is a thread that performs initial data exchanges to determine which document
the client is requesting, and then if the document manager exists, connects the client to the
existing document manager, or if not, retrieves the document (if it exists) from storage and
starts a document manager for the client.

It uses a |Map| from @Data.Map@ to keep track of document manager channels.

Note that this is actually stored in the @ConcurrencyControl.Types@ module to prevent cyclic import errors.

When the client first connects, the first message sent by the client is interpreted to be the name
of the document they are requesting. A thread is spawned (|ask|) to monitor for this message and,
after the message is sent, the client is directed back to the clientManager to be added to a
document manager.

\begin{code}%
data ClientManagerMsg = AddClient Client
                      | AddClientToDoc Client String
                      | RemoveDocument String
\end{code}

> clientManager :: Chan (ClientManagerMsg)
>               -> ThreadState ( M.Map String (Chan (DocumentManagerMsg))
>                              , Storage
>                              )
> clientManager inbox = do
>   (documents, storage) <- get
>   message <- grabMessage inbox
>   debug $ "clientManager recieved message: " ++ show message
>   case message of
>     RemoveDocument docName -> put (M.delete docName documents, storage)
>     AddClient client -> spawnThread () (ask client inbox) >> return ()
>     AddClientToDoc client docName -> do
>       case M.lookup docName documents of
>         Just chan -> NewClient client ~> chan
>         Nothing   -> do
>           snapshot <- io $ getDocument storage docName
>           case snapshot of
>             Nothing -> io $ closeClient client
>             Just v -> do
>               debug $ "spawning document manager for document: " ++ docName
>               dm <- spawnDocumentManager inbox docName v storage
>               NewClient client ~> dm
>               put (M.insert docName dm documents, storage)
>   clientManager inbox
>   where
>     ask client toCM = do
>       request <- io $ readFrame client
>       case request of
>         Left _        -> io $ closeClient client
>         Right docName -> AddClientToDoc client docName ~> toCM

\subsection{Port Listener}

The port listener is a very dumb thread that simply listens on the specified TCP port for incoming
connections, forwarding to the client manager.

> portListener :: Socket -> Chan (ClientManagerMsg) -> String -> Int -> IO ()
> portListener socket toCM location port = do
>   client <- acceptWeb socket location port
>   AddClient client ~> toCM
>   portListener socket toCM location port

\subsection{Server Bootstrap}

Finally, we have an IO action which kickstarts the server threads and initialises storage.

> server :: String -> Int -> IO ()
> server location port = withSocketsDo $ do
>   debug $ "Starting Opardum server on port " ++ show port ++ ".."
>   socket <- listenOn (PortNumber $ fromIntegral port)
>   debug "Listening for connections."
>   storage <- startStorage
>   debug "Initialized Storage Connection"
>   toCM <- newChan
>   spawnThread (M.empty, storage) $ clientManager toCM
>   debug "Created Client Manager"
>   portListener socket toCM location port

\end{document}
