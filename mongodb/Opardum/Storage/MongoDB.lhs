% Opardum
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: MongoDB Storage Driver}
\maketitle
\section{Introduction}

This module provides a permanent storage system for Opardum using a NoSQL database.

The storage system being used is MongoDB, a document database well suited to this application.

> module Opardum.Storage.MongoDB ( MongoDBStorage ()
>                                , MongoDBConfig (..)
>                                , defaults
>                                , mongoDB 
>                                ) where

> import Opardum.Storage
>
> import Database.MongoDB
> import Database.MongoDB.BSON
> import Data.ByteString.Lazy.UTF8
> import Data.Maybe
> import Network (PortID(..))

\section {Implementation}

The |MongoDB Storage| type is, internally, a @newtype@ around a MongoDB connection and a database name.

> newtype MongoDBStorage = MDB (Connection, ByteString)

Various MongoDB configuration options are specified here. Note that a port number of -1 indicates that the default port should be used.

> data MongoDBConfig = MongoDBConfig { location :: String
>                                    , port :: Int
>                                    , databaseName :: String
>                                    , slaveOK :: Bool
>                                    }
> defaults = MongoDBConfig { location = "127.0.0.1"
>                          , port = -1
>                          , slaveOK = False
>                          , databaseName = "opardum.documents" 
>                          }

Our instance of the @StorageDriver@ class provides MongoDB implementations for standard Opardum operations. See the @Storage@ module for details. 

> instance StorageDriver MongoDBStorage where
>    getDocument (MDB (conn,db)) str = do doc <- findOne conn db $ toBsonDoc [("name",toBson str)]
>                                         putStrLn $ "got " ++ show doc
>                                         return $ case doc of 
>                                            Nothing -> ""
>                                            Just v  -> fromBson $ fromJust $ "data" `lookup` fromBsonDoc v
 
>    updateDocument _ _ [] = return ()
>    updateDocument (MDB (conn,db)) str op = do update conn db [UFUpsert] (toBsonDoc [ ("name",toBson str) ])
>                                                       $ toBsonDoc [ ("name",toBson str)
>                                                                   , ("data",toBson op ) ]
>                                               return ()

Finally, the |mongoDB| action initializes a MongoDB connection using the provided configuration.

> mongoDB :: MongoDBConfig -> IO Storage
> mongoDB (MongoDBConfig l p db sok) = do 
>                                 conn <- (if p == -1 
>                                          then connect l 
>                                          else connectOnPort l $ PortNumber (fromIntegral p)) 
>                                        $ if sok then [SlaveOK] else []
>                                 return $ Storage $ MDB (conn, fromString db)


\end{document}
