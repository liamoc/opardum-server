% Opardum
% A Collaborative Code Editor
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Server}
\maketitle
\ignore{

> {-# LANGUAGE TypeFamilies, ExistentialQuantification  #-}

}
\section{Introduction}

In this module are contained singleton threads that manage initial client connections.

\section{Implementation}

> module Opardum.Server
>   ( module Opardum.Server.Types
>   , PortListener (..)
>   ) where
>
> import Opardum.Websockets
> import Opardum.Processes
> import Opardum.DocumentManager
> import Opardum.OperationalTransforms
> import Opardum.Server.Types
> import Opardum.Storage
> import Opardum.Archiver
> 
> import Data.Char
> import qualified Data.Map as M
> import Control.Monad.State
> import Control.Applicative ((<$>))
> import Network(Socket)

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
data ClientManager = ClientManager
\end{code}



> instance Process ClientManager where
>   type ProcessCommands ClientManager = ClientManagerMsg
>   type ProcessInfo ClientManager = (Storage, Archiver)
>   type ProcessState ClientManager =  (M.Map String (Chan DocumentManagerMsg))
>   continue _ = do
>      (storage, archiver) <- getInfo
>      inbox <- getInbox
>      documents <- getState
>      message <- grabMessage inbox
>      debug $ "clientManager recieved message: " ++ show message
>      case message of
>        RemoveDocument docName -> putState $ M.delete docName documents
>        AddClient client -> forkThread $ ask client inbox
>        AddClientToDoc client docName -> do
>          case M.lookup docName documents of
>            Just chan -> NewClient client ~> chan
>            Nothing   -> do
>              snapshot <- strToSnapshot <$> (io $ getDocument storage docName)
>              debug $ "spawning document manager for document: " ++ docName
>              dm <- io $ spawnDocumentManager inbox docName snapshot storage archiver
>              NewClient client ~> dm
>              putState $ M.insert docName dm documents
>      continue ClientManager
>      where
>        strToSnapshot :: String -> Op
>        strToSnapshot []  = []
>        strToSnapshot str = [Insert str]
>        ask client toCM = do
>          request <- readFrame client
>          case request of
>            Left _        -> closeClient client
>            Right docName -> if validName docName 
>                               then AddClientToDoc client docName ~> toCM
>                               else closeClient client
>        validName = all isAlphaNum

\subsection{Port Listener}

The port listener is a very dumb thread that simply listens on the specified TCP port for incoming
connections, forwarding to the client manager.

> data PortListener = PortListener
> instance Process PortListener where
>   type ProcessCommands PortListener = ()
>   type ProcessState PortListener = ()
>   type ProcessInfo PortListener = (Socket, Chan ClientManagerMsg, String, Int)
>   continue _ = do
>     (socket, toCM, location, port) <- getInfo
>     client <- io $ acceptWeb socket location port
>     AddClient client ~> toCM     
>     continue PortListener
>   nullChannel _ = True




\end{document}
