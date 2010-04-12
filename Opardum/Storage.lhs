% Opardum
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Storage}
\maketitle

\section{Introduction}

This module provides an abstract type, |Storage|, which provides an interface for permanent storage
of documents.

The storage engine currently used is Apache's @CouchDB@, written in Erlang, and very well suited to 
this application.

> module Opardum.Storage ( getDocument
>                        , updateDocument
>                        , startStorage
>                        , Storage()
>                        ) where

> import Opardum.OperationalTransforms
> import Database.CouchDB
> import Opardum.Transport
> import Text.JSON
> import Control.Monad.Trans

\section {Implementation}

The |Storage| type is, internally, a @newtype@ around a couchDB connection.

> newtype Storage = Storage CouchConn

Therefore, the constructor simply initiates a couchDB connection and wraps it.

If the database does not exist, it is created.

> startStorage = do 
>   conn <- createCouchConn "127.0.0.1" 5984 
>   return $ Storage conn

The couchDB database is called @Opardum@.

> database = db "Opardum"

We map Opardum documents to couchDB documents, so the name of the Opardum document must
also be a valid couchDB document. Hence, we have a fair bit of error checking in the next
function, |getDocument|, designed to retrieve a snapshot from the database.

> getDocument :: Storage -> String -> IO (Maybe Op)
> getDocument (Storage conn) str = runCouchDBWith conn $ 
>   if isDocString str then do
>     liftIO $ putStrLn $ "getting document: " ++ show str
>     doc' <- getDocPrim database (doc str)
>     case doc' of Nothing -> return $ Just []
>                  Just (_,_,v) -> 
>                   case (readJSON $ snd $ last $ v) of
>                      (Error _)-> return Nothing 
>                      (Ok res) -> return $ Just [Insert res ]
>   else return Nothing
> 

Finally, we have |updateDociument|, which is called by the archiver every 10 seconds to 
update the snapshot of a document.

> updateDocument :: Storage -> String -> Op -> IO ()
> updateDocument _ _ [] = return ()
> updateDocument (Storage conn) str [Insert op] = runCouchDBWith conn $ do
>   liftIO $ putStrLn $ "updating document: " ++ show op
>   v <- getDocPrim database (doc str)
>   case v of Nothing -> newNamedDoc database (doc str) (toJSObject [("data",op)]) >> return ()
>             Just (d,r,_) -> updateDoc database (d,r) (toJSObject [("data",op)]) >> return ()


\end{document}
