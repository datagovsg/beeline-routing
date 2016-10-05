

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

    webpack
    $ sbt run



TODO
====

3. More constraints on route feasibility
  a. Max detour time (hard coded time)
  b. Max detour time (as % of max route)
4. Other scoring functions?
5. Threshold annealing (Liwei)
6. Best regret route creation
