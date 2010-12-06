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
 * **Storage Backends** - by default, Opardum uses a null storage backend that 'forgets' a document as soon as all clients disconnect.
   * opardum-mongodb -- a MongoDB-based permanent storage backend 
   * opardum-file -- a flat file permanent storage backend
   * opardum-tokyo -- a Tokyo Cabinet permanent storage backend (TBA).
 * **Transport Protocols** - by default, Opardum uses web sockets to interface with the web client, however other transport protocols exist.
   * opardum-simplesockets -- A normal TCP sockets based transport protocol (TBA).

To compile and install Opardum with the [Haskell Platform](http://hackage.haskell.org/platform/), simply type:

    cabal configure
    cabal build
    cabal install

The Setup.hs script can also be used.

To build any of the sub-packages, simply run the same commands in their relevant directories.

Dependencies
------------

Dependencies of Opardum include:

  * Dyre
  * GHC
  * Network
  * MongoDB (for optional MongoDB backend)
  * Tokyo Cabinet (for optional Tokyo Cabinet backend)

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

Web Client
==========

A simple, probably buggy testing client has been implemented as a web page with an embedded [Mozilla Skywriter](https://mozillalabs.com/skywriter) editor. In order
to use the client, you will need to:

Run a web server to serve up the "Client" directory, such that:

  http://localhost/opardum/index.html

Refers to the index.html file located there. `localhost` can be substituted for any hostname.

HTML page and navigate there with a compliant browser such as Google Chrome. Note that advanced HTML5 features are required for Bespin, and WebSockets are used for the 
transport protocol. Hence Firefox 3.5 and any version of IE will not work.
