

Actions
=======

Setting up GraphHopper files
----------------------------

    $ tar xfz SG-gh.ghz

Or, from the graphhopper package, call
    
    graphhopper$ ./graphhopper.sh import <XXX.osm>

Refreshing the cache
--------------------

    $ sbt "run cache"

Running the routing
-------------------

    $ sbt "run route"

OR

    $ sbt run
