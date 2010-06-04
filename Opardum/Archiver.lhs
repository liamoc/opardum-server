% Opardum
% A Collaborative Code Editor
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Archiver}
\maketitle
\ignore{

> {-# LANGUAGE TypeFamilies, GADTs #-}

}
\section{Introduction}

A \emph{Archiver} |Process| is responsible for committing a document to the |Storage|. The most recent version of
a document (a |Snapshot|) is posted onto an |MVar| by the Document Manager.

We use an |MVar| rather than a channel because |MVar| allows the document manager to simply post the most recent 
snapshot into the MVar, not simply add it to a message queue. This means that the snapshot can
be updated multiple times but only the more recent snapshot is stored.

We run archiving operations asychronously as they are not needed for operational transforms, and
they may be slow given many transactions.

> module Opardum.Archiver ( ArchiverData (..), Archiver (..), ArchiverProcess (..) ) where
>
> import Opardum.Processes
> import Opardum.ConcurrencyControl
> import Opardum.Storage
>
> import Control.Monad.Trans ( MonadIO )
> import Control.Concurrent.MVar

\section{Implementation}

We simply define a subclass of |Process| that includes an initialization function that produces a (possibly null)
channel and the MVar, and kickstarting the archiver |Process| with the provided configuration and state.

> data ArchiverData = Archive Snapshot
>                   | ATerminate Snapshot
> class (Process a) => ArchiverProcess a where
>   type ArchiverConfig a
>   initArchiver :: (MonadIO m)
>                => a
>                -> ArchiverConfig a
>                -> Storage
>                -> DocName 
>                -> m (ChanFor a, MVar ArchiverData)

> data Archiver :: * where
>    Archiver :: ArchiverProcess a => a -> ArchiverConfig a -> Archiver



\end{document}
