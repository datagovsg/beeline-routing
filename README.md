Beeline Routing
===============

Welcome! There are three sub-projects in this repository:

1. Geospatial point clustering algorithm (`src/main/scala/sg/beeline/clustering`)
2. Global routing algorithm (`src/main/sg/beeline/ruinrecreate/BasicRoutingAlgorithm.scala`)
3. Localized routing algorithm (`src/main/sg/beeline/web/Web.scala`)

In addition, we have a utility library to fetch OneMap and Data Mall bus heading data (`fetch-bus-stops-with-headings`).


Geospatial point clustering algorithm
-------------------------------------

This is a greedy clustering algorithm for points on a 2D plane. In typical clustering
algorithms, we do not specify cluster size. However, in Beeline we are limited by practical
walking distance to the nearest pick-up point. Hence for this algorithm, we allow you
to specify the maximum cluster size. Consequently, you won't be able to control the number of
clusters.

The algorithm will always create clusters centred on a data point. This works reasonably
well for our purposes because people's dwellings tend to be scattered relatively uniformly
in a town, while pathological cases (like dwellings arranged in a circle) are rare.

It takes the first data point with the largest number of close, unclustered neighbours.
These neighbours and the data point form the first cluster. Then we repeat for the next data
point with the largest number of still unclustered neighbours, until there are no unclustered
data points left.

This algorithm is useful for identifying O/D hotspots, but we do not have a public demonstration
of this algorithm yet.

Global routing algorithm
------------------------

This is a re-implementation of the ruin-recreate meta-algorithm. Unlike [Jsprit](https://jsprit.github.io/)
, we allow requests (passengers) to be served at one of multiple pre-set points
(bus stops) in its vicinity (for example, 300m).

We do not have a public demonstration of this. Moreover, because this does not fit our use case, we will not
be making updates to it.

Localized routing algorithm
---------------------------

This uses only the recreate step of the ruin-recreate algorithm to create exactly **one** route
from a seed (an O/D pair) and suggestions similar to it, that is, other suggestions
where both the origin and destination are less than a fixed distance (e.g. 3km) from
that of the seed.

This is the version available at https://www.beeline.sg/suggest.html.

To use it, please follow the following flow:
1. Suggest a route, and verify your email
2. When your suggestion has appeared in "Your previous suggestions", view its details.
   by clicking on the icon to the left of the date.
3. Under "Generate Some Routes", click the button that says "Ok, show me what you've got"

Bus Stop Heading
----------------
The GraphHopper routing library allows us to specify the heading at the origin and at the
destination. On small roads, this prevents the routing algorithm from telling the bus
to make a U-turn (sometimes illegal, and really difficult with 40-seater buses) at
a waypoint.

However, neither SLA's OneMap nor LTA's Data Mall encodes the heading of the bus stop.
To derive the heading of the bus stop, we take successive stop pairs on existing public bus
routes, route them using GraphHopper (we assume it's trivial enough to route
between successive stops without having to do illegal U-turns) and take the initial and final
headings from the route path to determine the heading of the bus stop.
