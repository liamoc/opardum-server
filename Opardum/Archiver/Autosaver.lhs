% Opardum
% A Collaborative Code Editor
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Autosaver Archiver}
\maketitle
\ignore{

> {-# LANGUAGE TypeFamilies #-}

}
\section{Introduction}
The autosaver is an asynchronous thread that, every 10 seconds (or a user-defined time), 
commits new documents to some permananent data storage, as defined in the @Storage@ module. 
It implements the @Archiver@ interface. It reads data from an |MVar| shared with the document 
manager. 

> module Opardum.Archiver.Autosaver 
>           ( Autosaver (..)
>           , defaultInterval
>           , autosaver
>           ) where
>
> import Opardum.ConcurrencyControl
> import Opardum.Archiver
> import Opardum.Storage
> import Opardum.Processes
> import Opardum.OperationalTransforms
>
> import Control.Concurrent(threadDelay)
> import Control.Concurrent.MVar

\section{Implementation}

First, we define a |Process|, |Autosaver|, which performs the required actions. It state includes
the |MVar| with the document, the |Storage| type, a document name, and the time it should wait 
between saves.


> type Interval = Int
>
> data Autosaver = Autosaver   
> instance Process Autosaver where
>   type ProcessInfo Autosaver = ( MVar ArchiverData
>                                , Storage
>                                , DocName
>                                , Interval
>                                )
>   type ProcessState Autosaver = ()
>   type ProcessCommands Autosaver = ()
>   continue me = do
>     (mv, store, docName, wait) <- getInfo
>     io $ threadDelay wait
>     msg <- io $ takeMVar mv
>     debug $ "committing"
>     case msg of
>        Archive [Insert doc]    -> do io $ updateDocument store docName doc; continue me
>        ATerminate [Insert doc] -> io $ updateDocument store docName doc
>        Archive []              -> do io $ updateDocument store docName ""; continue me
>        ATerminate []           -> io $ updateDocument store docName ""
>   nullChannel _ = True
>

We also make |Autosaver| an implementation of the subclass |ArchiverProcess|, specifying its configuration
type to be the time it should wait between saving.

> instance ArchiverProcess Autosaver where
>   type ArchiverConfig Autosaver = Int -- Interval
>   initArchiver _ wait storage docName = do
>        mv <- io $ (newEmptyMVar :: IO (MVar ArchiverData))
>        toA <- runProcess' Autosaver (mv, storage, docName, wait)
>        return (toA, mv)

> defaultInterval :: Int
> defaultInterval = 100000

Finally, we provide a simple function to make the configuration file look neater.

> autosaver :: Int -> Archiver
> autosaver = Archiver Autosaver


\end{document}
