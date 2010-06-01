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

In this module are contained singleton threads that manage initial client connections.

\section{Implementation}

> module Opardum.Server
>   ( clientManager
>   , ClientManagerMsg (..)
>   , portListener
>   ) where
>
> import Opardum.Websockets
> import Opardum.ConcurrentChannels
> import Opardum.ConcurrencyControl
> import Opardum.OperationalTransforms
> import Opardum.Storage
> 
> import Data.Char
> import qualified Data.Map as M
> import Control.Monad.State

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

> clientManager :: Storage a
>               => Chan (ClientManagerMsg)
>               -> ThreadState ( M.Map String (Chan (DocumentManagerMsg)), a)
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
>           debug $ "spawning document manager for document: " ++ docName
>           dm <- spawnDocumentManager inbox docName (strToSnapshot snapshot) storage
>           NewClient client ~> dm
>           put (M.insert docName dm documents, storage)
>   clientManager inbox
>   where
>     strToSnapshot []  = []
>     strToSnapshot str = [Insert str]
>     ask client toCM = do
>       request <- io $ readFrame client
>       case request of
>         Left _        -> io $ closeClient client
>         Right docName -> if validName docName 
>                            then AddClientToDoc client docName ~> toCM
>                            else io $ closeClient client
>     validName = all isAlphaNum

\subsection{Port Listener}

The port listener is a very dumb thread that simply listens on the specified TCP port for incoming
connections, forwarding to the client manager.

> portListener socket toCM location port = do
>   client <- acceptWeb socket location port
>   AddClient client ~> toCM
>   portListener socket toCM location port


\end{document}
