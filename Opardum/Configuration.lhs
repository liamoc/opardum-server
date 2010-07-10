% Opardum
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Configuration }
\maketitle
\ignore{

> {-# OPTIONS_GHC -fno-warn-name-shadowing #-}

}
\section{Introduction}

Opardum has been designed to use Haskell as its main configuration language. The library \emph{Dyre}, by Will Donnelly, 
rovides this functionality basically for free.

For more information, see the Dyre hackage page at:

 @http://hackage.haskell.org/package/dyre@

\section{Implementation}

> module Opardum.Configuration 
>   ( Config (..)
>   , defaultConfig
>   , defaultLocation
>   , defaultPort
>   , opardum
>   ) where

> import qualified Config.Dyre as Dyre
> import Debug.Trace

> import Opardum.Server
> import Opardum.Storage
> import Opardum.Storage.NullStorage
> import Opardum.Processes
> import Opardum.Archiver
> import Opardum.Archiver.Autosaver
> import Network
> import qualified Data.Map as M

\subsection{Server Configuration}

This section defines the server configuration type and its default values, which uses the default Null storage driver, and the autosaver archiver.

> data Config = Config { location :: String
>                      , port :: Int
>                      , storage :: IO Storage
>                      , archiver :: Archiver
>                      }

> defaultConfig :: Config
> defaultConfig = Config defaultLocation defaultPort nullStorage (autosaver defaultInterval)

> defaultLocation :: String
> defaultLocation = "localhost"

> defaultPort :: Int
> defaultPort = 9988


\subsection{Server Bootstrap}

|bootstrap| is simply an IO action that starts server threads and initializes storage.

> bootstrap :: Config -> IO ()
> bootstrap (Config location port startStorage archiver) = withSocketsDo $ do
>   debug $ "Starting Opardum server on port " ++ show port ++ ".."
>   socket <- listenOn (PortNumber $ fromIntegral port)
>   debug "Listening for connections."
>   storage <- startStorage
>   debug "Initialized Storage"
>   toCM <- runProcess (storage, archiver) M.empty
>   debug "Created Client Manager"
>   switchTo (undefined :: PortListener) (socket, toCM, location, port) (undefined)

\subsection{Dyre Glue}

This function is applied by configuration files with a |Config| in order to configure Opardum.

> opardum :: Config -> IO ()
> opardum = Dyre.wrapMain $ Dyre.defaultParams
>         { Dyre.projectName = "opardum"
>         , Dyre.realMain    = bootstrap
>         , Dyre.showError   = \a s -> trace s a
>         , Dyre.hidePackages = ["transformers","monads-fd"]
>         }

\end{document}
