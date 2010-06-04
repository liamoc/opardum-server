% Opardum
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Flat File Storage Driver}
\maketitle
\section{Introduction}

This module provides a permanent storage system for Opardum using flat files.

> module Opardum.Storage.File ( FileStorage()
>                             , flatFile 
>                             ) where

> import Opardum.Storage
>
> import System.Directory
> import System.IO
> import Control.Exception
> import System.FilePath

\section {Implementation}

The |FileStorage| type is, internally, a @newtype@ around the directory in which the files are located.

> newtype FileStorage = F String

Our instance of the @StorageDriver@ class provides simple flat file implementations for get and update Opardum operations.

> instance StorageDriver FileStorage where
>     getDocument (F dir) n = let filename = (addTrailingPathSeparator dir) `replaceFileName` n
>                             in do v <- doesFileExist filename
>                                   if v then readFile filename
>                                        else return ""
>     updateDocument (F dir) n op = let filename = (addTrailingPathSeparator dir) `replaceFileName` n 
>                                    in writeFile filename op                                     


Finally, the |flatFile| action creates a |FileStorage| type given a directory.

> flatFile :: FilePath -> IO Storage
> flatFile dir = do v <- doesDirectoryExist dir
>                   if v then return $ Storage $ F dir
>                        else throw $ InvalidConfiguration "Directory does not exist!"


\end{document}
