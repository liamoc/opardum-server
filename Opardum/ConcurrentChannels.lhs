% Opardum
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Concurrent Channels}
\maketitle

\ignore{

> {-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}

}
\section{Introduction}

This utility module introduces a variety of convenience functions that are used for concurrent control 
throughout Opardum.

At the heart of it is the |ThreadState| monad, which is the state transformer |stateT| over |IO|, designed to make it
easier to write threads with thread-local state.

> module Opardum.ConcurrentChannels 
>   ( io
>   , debug
>   , Opardum.ConcurrentChannels.newChan
>   , (~>)
>   , grabMessage
>   , ThreadState()
>   , spawnThread
>   , C.Chan()
>   ) where
>
> import qualified Control.Concurrent.Chan as C
> import Control.Concurrent
> import Control.Monad.Trans
> import Control.Monad.State

\section {Implementation}

First we define a convenience function because |liftIO| is annoying to type:

> io :: (MonadIO m) => IO a -> m a
> io = liftIO

As well as a version of |putStrLn| used for debugging in any |MonadIO|.

> debug :: (MonadIO m) => String -> m ()
> debug = io . putStrLn

\subsection {ThreadState Monad}

In order for thread-local storage, we shall define a state transformer over IO, which is our |ThreadStateM| 
type. 

We will use GHC's @newtype@ @deriving@ feature to automatically make |ThreadStateM| an instance of |Monad|, 
|Functor|, and |MonadIO|, based on the definitions for |StateT|. Furthermore, we write manual forwarding
instances for |MonadState|, as it has 2-arity which prevents newtype deriving.

> newtype ThreadStateM v s = ThreadState (StateT v IO s) 
>    deriving (Monad, Functor, MonadIO)

> instance MonadState v (ThreadStateM v) where
>   get = ThreadState get
>   put = ThreadState . put

Finally, seeing as the return type from the |ThreadStateM| monad does not usually matter, we define the
unit return to be |ThreadState|.

> type ThreadState v = ThreadStateM v ()

We introduce a runner for this monad, which is a wrapper around both |forkIO|, to spawn the thread, and |runStateT|,
to provide initial state.

> spawnThread :: MonadIO m => s -> ThreadState s -> m ThreadId
> spawnThread state (ThreadState v) = io $ forkIO $ (runStateT v state >> return ())

\subsection {Channels}

The final piece of the puzzle is some wrappers around the @Control.Concurrent.Chan@ library for use
in |ThreadState|.

First, is the "send to" relation, |~>|, which is analogous to the |writeChan| function, lifted for any
|MonadIO|.


> (~>) :: MonadIO m => a -> Chan a -> m ()
> (~>) = flip $ (io .) . C.writeChan

We also have an analog for the |readChan| function, |grabMessage|.

> grabMessage :: MonadIO m => Chan a -> m a
> grabMessage = io . C.readChan

And, predictably, an analog for the |newChan| function, which goes by the same name.

> newChan :: MonadIO m => m (Chan a)
> newChan = io $ C.newChan

\end{document}
