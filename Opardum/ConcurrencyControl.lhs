% Opardum
% A Collaborative Code Editor
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Concurrency Control}
\maketitle

\section{Introduction}

Opardum makes use of Operational Transforms (see the @OperationalTransforms@ module) in order to
adjust operations that happen concurrently, but this does not provide a very broad concurrent context.

Therefore, this module focuses on:
\begin{itemize}
\item Error checking and client validation
\item Where the operational transforms are used in the server
\item Managing a server op list for each client
\end{itemize}

\subsection{Operation Processing}

When an op comes into the server, three things happen.
\begin{description}
\item[Transformation]
The op is transformed against every op that the client has not yet seen to bring it up to date.
The client sends with the op a number, $d_c$, which is specified by the client as the amount of
operations the client has received (and transformed) from the server since the client last sent an
operation, or since the client connected if it has not yet sent any operations. The server maintains
a list of the operations which it has sent to each client, and so it can simply drop the first $d_c$
elements of this list to determine which operations the incoming operation must be transformed against.
\item[Validation]
The incoming op is applied to the snapshot of the entire document via composition (|+>|). If the
op does not compose, then the op is discarded as erroneous and the client is rejected (disconnected).
\item[Transmission]
The op is stored in the operation list for each client (except the client that sent the operation),
and is sent to each client along with another number, $d_s$, which is the number of operations the server
has seen from the client since the server last dispatched ops to that particular client.
\end{description}

In this module, we define a set of functions that fulfill these three responsibilities, and threads
to manage state for each document.

> module Opardum.ConcurrencyControl
>   ( module Opardum.ConcurrencyControl.Types
>   , Snapshot
>   , spawnDocumentManager
>   )
>   where
>
> import Opardum.OperationalTransforms
> import Opardum.ConcurrentChannels
> import Opardum.ClientRegistry
> import Opardum.Websockets
> import Opardum.ConcurrencyControl.Types
> import Opardum.Storage
>
> import Control.Exception
> import Control.Concurrent.MVar
> import Control.Concurrent(threadDelay)
> import Control.Monad.State
> import Data.List
> import Data.Maybe (fromMaybe)
> import Prelude hiding (catch)
> import qualified Data.Map as M

\section{Implementation}
\subsection{Data types}
A packet is the data that is transmitted by both client and server to send operations over the wire.
It is simply a pair of the operation and the number $d$ as described in section 1.1.

Note that this is actually stored in the |Messages| module to get around circular module imports.

\begin{code}%
 type Packet = (Int, Op)
\end{code}

The document, or snapshot, as stored on the server side, is represented as one large operation that is
the composition of all ops so far.

> type Snapshot = Op

\emph{Note}: The snapshot is represented differently in the client.

\subsection{Functions}

First we will define a function to fullfill the first responsibility, transformation:

> incomingTransform :: Int        -- The value $d_c$
>                   -> [Op]        -- The current per-client operation list
>                   -> Op          -- The incoming operation
>                   -> ( Op        -- The transformed operation to be applied.
>                      , [Op] )   -- The new operation list for the client
> incomingTransform d oplist op = mapAccumL (curry t) op $ drop d oplist

Here the incoming op is treated as an accumulator in a |mapAccum| operation, such that
the op is transformed against all operations not seen by the client, producing a new
operation list in the process, which should replace the old list in the document manager
thread for that client. The final transformed operation is then applied to the snapshot
for verification and storage.

\medskip

Next we will define an IO action for updating the snapshot and verification.

If the operation is invalid, this IO action returns |Nothing| which is an indication that
the client may be malicious or malfunctioning, and should be disconnected.


> updateSnapshot :: Op -> Snapshot -> IO (Maybe Snapshot)
> updateSnapshot op doc = return $ fmap normalize $ doc +> op

\section{Threads}

\subsection{Document Manager}
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

\medskip

For information about the concurrency library used here, please see the @ConcurrentChannels@
module.

\medskip

We define the input message to a document manager to include a message for when a client has connected,
a message for when a client has disconnected, and a message when a client sends an op.

\emph{Note:} The |DocumentManagerMsg| type is actually in the undocumented @Messages@ module, simply
to get around circular module imports.

\begin{code}%
 data DocumentManagerMsg = NewClient Client
                         | NewOp Client Packet
                         | RemoveClient Client
\end{code}

All communication to clients is handled by the |ClientRegistry| type, so most of the client-management
logic in this thread is dumb. The salient part of the logic is the handler for the |NewOp| message.

It would be worthwhile to consult the |ClientRegistry| documentation for information about many
functions called in this thread.

The document manager only talks to client threads through the |ClientRegistry|, hence it requires
only its input channel as an argument, as well as a return channel to the client manager that
created it.

Its state consists of: the running document snapshot, the |ClientRegistry| (which is a mutable IO
structure), the map of |Client|s to their operation lists and the document name.

So that the archiver thread can read the most recent |Snapshot|, we also make it available in an MVar
which is read by the archiver every 10 seconds for storage.

The document manager (and the archiver) terminate if all clients disconnect.

> documentManager :: Chan (DocumentManagerMsg)
>                 -> Chan (ClientManagerMsg)
>                 -> ThreadState ( MVar ArchiverData
>                                , Snapshot
>                                , ClientRegistry
>                                , M.Map Client [Op]
>                                , String
>                                )
> documentManager inbox toCM = do
>   (mv, doc, cr, cl, docName) <- get
>   currentMessage <- grabMessage inbox
>   debug $ "documentManager received:" ++ show currentMessage
>   numCs <- io $ numClients cr
>   if numCs == 0 && not (isNewClient currentMessage)
>    then do
>      io $ tryTakeMVar mv
>      io $ putMVar mv $ ATerminate doc
>      RemoveDocument docName ~> toCM
>    else do
>      (cl',doc') <- case currentMessage of
>        NewClient c     -> do io $ do createClient cr c
>                                      sendSnapshot cr doc c
>                              return (cl, doc)
>        RemoveClient c  -> do io $ removeClient cr c
>                              return (M.delete c cl, doc)
>        NewOp c (d_c,o) -> do io $ let opList = fromMaybe [] (M.lookup c cl)
>                                       (o', opList') = incomingTransform d_c opList o
>                                    in do v <- updateSnapshot o' doc
>                                          debug $ "::" ++ show o'
>                                          case v of Nothing -> do removeClient cr c
>                                                                  return (M.delete c cl, doc)
>                                                    Just d' -> do let cl' = M.insert c opList' cl
>                                                                  sendOp cr o' c
>                                                                  tryTakeMVar mv
>                                                                  putMVar mv (Archive d')
>                                                                  return (cl',d')
>      put (mv, doc', cr, cl', docName)
>      documentManager inbox toCM
>    where isNewClient (NewClient _) = True
>          isNewClient _             = False

Spawning a document manager initializes it with some simple empty state. Seeing as the document
manager and the archiver are fairly tightly bound, this is also used to spawn an archiver.
The document manager is responsible for ensuring the archiver terminates.

> spawnDocumentManager toCM docName doc storage = do
>   c <- newChan
>   mv <- io $ newEmptyMVar
>   spawnThread (storage, docName) (archiver mv)
>   cr <- io $ createRegistry c
>   spawnThread (mv, doc, cr , M.empty, docName) (documentManager c toCM)
>   return c

\subsection{Archiver}

The archiver is an asynchronous thread that, every 10 seconds, commits new documents to some
permananent data storage, as defined in the @Storage@ module. It reads data from an |MVar| shared
with the document manager. We use an |MVar| rather than a channel because |MVar| allows the
document manager to simply post the most recent snapshot into the MVar, not simply add it to
a message queue. This means that the snapshot can be updated multiple times but only the more
recent snapshot is stored.

We run archiving operations asychronously as they are not needed for operational transforms, and
they may be slow given many transactions.

> data ArchiverData = Archive Snapshot
>                   | ATerminate Snapshot

> archiver :: MVar ArchiverData -> ThreadState (Storage, String)
> archiver mv = do
>   (store, docName) <- get
>   io $ threadDelay 10000000
>   msg <- io $ takeMVar mv
>   case msg of
>      Archive doc    -> do io $ updateDocument store docName doc; archiver mv
>      ATerminate doc -> io $ updateDocument store docName doc

\end{document}
