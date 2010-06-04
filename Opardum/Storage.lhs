% Opardum
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Storage}
\maketitle
\ignore{

> {-# LANGUAGE DeriveDataTypeable, GADTs #-}

}
\section{Introduction}

Opardum is designed so that a variety of storage backends can be used, including MongoDB and flat files.

To achieve this end, we define a type, |Storage|, which encapsulates all the operations that Opardum requires - 
simply, retrieval and storage of documents.

> module Opardum.Storage where

> import Control.Exception
> import Data.Typeable

\section{Implementation}

We define a typeclass |StorageDriver| over the storage type a.

> class StorageDriver a where

We define the two operations required by Opardum. |getDocument| is expected to return an empty string if the document does not exist. It takes a storage handle
and a document name and produces the document required in string form.

>    getDocument :: a -> String -> IO String

|updateDocument| either creates or updates a document such that it contains the specified string contents. The same document will never be written to 
concurrently, however multiple updates to different documents may occur concurrently. Hence, concurrent access to storage is required, but not concurrent 
access to each individual document.

>    updateDocument :: a -> String -> String -> IO ()

We also define an IO exception for these actions to throw if they fail (however any exception will be caught):

> data StorageException = InvalidConfiguration String 
>                       | GeneralStorageFailure String 
>                       deriving (Show,Typeable)
> instance Exception StorageException

Finally, we define a GADT wrapper around the class to completely encapsulate storage drivers as a type rather than a constraint.

> data Storage where
>    Storage :: StorageDriver a => a -> Storage

> instance StorageDriver Storage where
>    getDocument (Storage a) = getDocument a
>    updateDocument (Storage a) = updateDocument a


\end{document}
