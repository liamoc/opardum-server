% Haskellpad
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Haskellpad: Storage}
\maketitle

\section{Introduction}

This module provides an abstract type, |Storage|, which provides an interface for permanent storage
of documents.

The storage engine currently used is Apache's @CouchDB@, written in Erlang, and very well suited to 
this application.

> module Haskellpad.Storage ( getDocument
>                           , updateDocument
>                           , startStorage
>                           , Storage()
>                           ) where

> import Haskellpad.OperationalTransforms
> import Database.CouchDB
> import Control.Exception (try, ErrorCall(..), catch)
> import Prelude hiding (catch)
> import Haskellpad.Transport
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

The couchDB database is called @haskellpad@.

> database = db "haskellpad"

> getDocument :: Storage -> String -> IO (Maybe Op)
> getDocument (Storage conn) str = (runCouchDBWith conn $ do
>   liftIO $ putStrLn $ "getting document: " ++ show str
>   doc <- getDocPrim database (doc str)
>   case doc of Nothing -> return $ Just []
>               Just (_,_,v) -> 
>                 case (readJSON $ snd $ head $ v) of
>                    (Error _)-> return Nothing 
>                    (Ok str) -> return $ Just [Insert str ]
>   ) `catch` (\(ErrorCall _) -> return Nothing)
> 

> updateDocument :: Storage -> String -> Op -> IO ()
> updateDocument _ _ [] = return ()
> updateDocument (Storage conn) str [Insert op] = runCouchDBWith conn $ do
>   liftIO $ putStrLn $ "updating document: " ++ show op
>   v <- getDocPrim database (doc str)
>   case v of Nothing -> newNamedDoc database (doc str) (toJSObject [("data",op)]) >> return ()
>             Just (d,r,_) -> updateDoc database (d,r) (toJSObject [("data",op)]) >> return ()


\end{document}
