% Opardum
% A Collaborative Code Editor
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Document Manager}
\maketitle
\ignore{

> {-# LANGUAGE TypeFamilies #-}

}
\section{Introduction}
The thread which manages per-document state is called the Document Manager, which is responsible for:

\begin{description}
\item[1.Managing incoming and outgoing clients] It uses the |ClientRegistry| ADT which is defined in the
@ClientRegistry@ module, to keep track of currently connected clients. This responsibility includes
sending a snapshot to newly connected clients.

\item[2.Maintaing the running snapshot of the document] It keeps the running snapshot of the document, and
is responsible for maintaining document integrity via composition, and disconnecting misbehaving clients.

\item[3.Handling new ops from clients] In particular, this responsibility involves transforming new ops and
distributing the transformed op to other clients.
\end{description}

> module Opardum.DocumentManager 
>           ( module Opardum.DocumentManager.Types
>           , spawnDocumentManager
>           ) where
> import Opardum.DocumentManager.Types
> import Opardum.Processes
> import Opardum.ClientRegistry
> import Opardum.Websockets
> import Opardum.Storage
> import Opardum.ConcurrencyControl
> import Opardum.OperationalTransforms
> import Opardum.Archiver
> import Opardum.Server.Types
>
> import Control.Exception
> import Control.Concurrent.MVar
> import Control.Monad.State
> import Data.Maybe
> import qualified Data.Map as M

\section{Implementation}
We define the input message to a document manager to include a message for when a client has connected,
a message for when a client has disconnected, and a message when a client sends an op.

\emph{Note:} The |DocumentManagerMsg| type is actually in the undocumented @DocumentManager.Types@ module, 
simply to get around circular module imports.

\begin{code}%
 data DocumentManagerMsg = NewClient Client
                         | NewOp Client Packet
                         | RemoveClient Client
\end{code}

All communication to clients is handled by the |ClientRegistry| type, so most of the client-management
logic in this thread is dumb. The salient part of the logic is the handler for the |NewOp| message.

It would be worthwhile to consult the |ClientRegistry| documentation for information about many
functions called in this thread.

So that the archiver thread can read the most recent |Snapshot|, we also make it available in an MVar
which is read by the archiver for storage.

The document manager (and the archiver) terminate if all clients disconnect. 

\begin{code}%
 -- Actually in the Hidden Types module.
 data DocumentManager = DocumentManager
\end{code}



> instance Process DocumentManager where

The state consists of: the running document snapshot and the map of |Client|s to their operation
lists. The document name, the |ClientRegistry| (which is a mutable IO structure), the |MVar| to
the archiver and channel to the client manager are stored in the Info portion for read-only
access.

>   type ProcessInfo DocumentManager =  ( Chan ClientManagerMsg
>                                       , MVar ArchiverData
>                                       , ClientRegistry
>                                       , DocName
>                                       )
>   type ProcessState DocumentManager = ( Snapshot
>                                       , M.Map Client [Op]
>                                       )
>   type ProcessCommands DocumentManager = DocumentManagerMsg
>   continue _ = do
>     inbox <- getInbox
>     (toCM, mv, cr, docName) <- getInfo
>     (doc, cl) <- getState
>     currentMessage <- grabMessage inbox

Once all clients disconnect, the storage is committed, and the archiver and document manager will
terminate.

>     numCs <- io $ numClients cr
>     if numCs == 0 && not (isNewClient currentMessage)
>      || numCs - 1 == 0 && (isRemoveClient currentMessage)
>      then do
>        updateMVar mv $ ATerminate doc
>        RemoveDocument docName ~> toCM
>      else do
>        (cl',doc') <- case currentMessage of

The messages for new or departing clients simply forward  to their |ClientRegistry| equivalents.

>          NewClient c     -> do io $ do createClient cr c
>                                        sendSnapshot cr doc c
>                                return (cl, doc)
>          RemoveClient c  -> do io $ removeClient cr c
>                                return (M.delete c cl, doc)

The |NewOp| message performs the |incomingTransform| operation defined in the @ConcurrencyControl@
module, bringing the op to a state which can be applied to the current snapshot.


>          NewOp c (d_c,o) -> do io $ let opList = fromMaybe [] (M.lookup c cl)
>                                         (o', opList') = incomingTransform d_c opList o
>                                      in do v <- updateSnapshot o' doc
>                                            debug $ "::" ++ show o'

If composition fails (i.e the op is invalid), the client is removed.

>                                            case v of Nothing -> do removeClient cr c
>                                                                    return (M.delete c cl, doc)

Otherwise, the ops are transmitted to every other client, and updated for storage in the |MVar|.

>                                                      Just d' -> do let cl' = M.insert c opList' cl
>                                                                    sendOp cr o' c
>                                                                    updateMVar mv $ Archive d'
>                                                                    return (cl',d')
>
>        putState (doc', cl')
>        continue DocumentManager
>      where isNewClient (NewClient _) = True
>            isNewClient _             = False
>            isRemoveClient (RemoveClient _) = True
>            isRemoveClient _                = False
>            updateMVar mv v = do
>                    io $ tryTakeMVar mv
>                    io $ putMVar mv v

Spawning a document manager initializes it with some simple empty state. Seeing as the document
manager and the archiver are fairly tightly bound, this is also used to spawn an archiver.
The document manager is responsible for ensuring the archiver terminates.

> spawnDocumentManager toCM docName doc storage (Archiver archiver config) = do
>   (toAR, mv) <- initArchiver archiver config storage docName  
>   c <- createChannel DocumentManager
>   cr <- io $ createRegistry c
>   runProcessWith DocumentManager c (toCM, mv, cr, docName) (doc, M.empty)
>   return c

\end{document}
