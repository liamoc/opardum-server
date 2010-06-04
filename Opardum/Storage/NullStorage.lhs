% Opardum
% A Collaborative Code Editor 
% Written by Liam O'Connor-Davis with assistance from the
% rest of the Google Wave Team.
% Released under BSD3 License
%include lhs.include
\begin{document}
\title{Opardum: Null Storage Driver}
\maketitle
\section{Introduction}

This module defines a simple no-op storage driver for use as the default. The effect of this driver is that documents are lost when all clients disconnect.

> module Opardum.Storage.NullStorage where
>
> import Opardum.Storage
>
> data NullStorage = NullStorage
>
> instance StorageDriver NullStorage where
>    getDocument _ _      = return ""
>    updateDocument _ _ _ = return ()

> nullStorage :: IO Storage
> nullStorage = return (Storage $ NullStorage)

\end{document}
