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
>   ( Snapshot
>   , Packet
>   , DocName
>   , incomingTransform
>   , updateSnapshot
>   )
>   where
>
> import Opardum.OperationalTransforms
>
> import Data.List
> import Control.Monad.Trans

\section{Implementation}
\subsection{Data types}
A packet is the data that is transmitted by both client and server to send operations over the wire.
It is simply a pair of the operation and the number $d$ as described in section 1.1.

> type Packet = (Int, Op)

The document, or snapshot, as stored on the server side, is represented as one large operation that is
the composition of all ops so far.

> type Snapshot = Op

Also, a DocName is simply a more informative name for a string.

> type DocName = String

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

Finally we will define an IO action for updating the snapshot and verification.

If the operation is invalid, this IO action returns |Nothing| which is an indication that
the client may be malicious or malfunctioning, and should be disconnected.

> updateSnapshot :: MonadIO m => Op -> Snapshot -> m (Maybe Snapshot)
> updateSnapshot op doc = return $ fmap normalize $ doc +> op

\end{document}
