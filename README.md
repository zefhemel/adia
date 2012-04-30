Adia is a web language developed using Clojure. It currently uses MongoDB for data storage.

Installation instructions
=========================

First, install [leiningen](http://github.com/technomancy/leiningen), e.g. by [following these instructions](http://zef.me/2470/building-clojure-projects-with-leiningen). Then, install [MongoDB](http://www.mongodb.org) if you haven't already. Launch the MongoDB server.

Then, `cd` to the directory where you keep your git repositories, then clone
Adia's git repository and let leiningen download all its dependencies:

    git clone git://github.com/zefhemel/adia.git
    cd adia
    lein deps

Edit the `bin/adia` script and change the ADIA_PATH variable, e.g.:

    ADIA_PATH=~/mygit/adia

Make sure the `adia` script is executable and on your PATH (or just type in the path to the script every time, if you prefer):

    chmod +x bin/adia
    export PATH=$PATH:~/mygit/adia/bin

Now you can create a new project somewhere:

    adia new

This will ask for a project name, for instance `helloworld`. Then cd into that directory and run it:

    cd helloworld
    adia run

A server will now be started at port 8080, go to [http://localhost:8080](http://localhost:8080) to see your application running. 

Examples
========

See `examples/wiki` for a very simple wiki application. You can run this application similarly:

    cd ~/mygit/adia/examples/wiki
    adia run

Documentation
=============

Sorry, the documentation is *very* limited right now.