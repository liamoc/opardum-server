Opardum
=======

Opardum is an operational transforms based server for collaborative document editing, written entirely in literate haskell.

Generating Literate Sources
===========================

You will need lhs2tex, and a pdflatex implementation.

    cd lhs
    ./generate.sh


Building
========

Opardum is made up of several packages:

 * opardum -- the main server implementation
 * opardum-mongodb -- a MongoDB storage backend 
 * opardum-file (TBA) -- a flat file storage backend

To compile and install Opardum with the [Haskell Platform](http://hackage.haskell.org/platform/), simply type:

   cabal configure
   cabal build
   cabal install

The Setup.hs script can also be used.

To build the mongoDB package, simply run the same commands from the mongodb/ directory. 

Dependencies
------------

Dependencies of Opardum include:

  * Dyre
  * GHC
  * Network
  * MongoDB (for optional MongoDB backend)

Configuring
===========

Opardum is configured using the *dyre* package, enabling it to be configured in the Haskell language itself, in a way similar to
[xmonad](http://xmonad.org).

The configuration file will be located in $XDG_CONFIG_HOME/opardum/opardum.hs. On Linux systems, this is ~/.config/opardum/opardum.hs

A simple configuration file is as follows:

    import Opardum.Configuration

    main = opardum defaultConfig

By default, Opardum runs on port 9988, with the websocket protocol, and a null storage backend (document state is lost when all clients disconnect).

A more useful configuration file is as follows (using the mongoDB storage backend to enable permanent storage)

    import Opardum.Configuration
    import Opardum.Storage
    import qualified Opardum.Storage.MongoDB as M

    main = opardum defaultConfig { storage = M.mongoDB M.defaults }

See the file Opardum/Configuration.lhs for a full description of options.

Running opardum with a changed config file will cause it to recompile.
