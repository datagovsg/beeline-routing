# Update Bus Stop Headings

Every so often, we will need to update the list of bus stops in Beeline-routing.

However, we need more than just the bus-stop lat lngs. For GraphHopper to work effectively, we also need the direction
in which the bus enters and leaves each bus stop.

This script fetches bus stops from DataMall and GraphHopper, and uses whatever information is
available in there to determine the heading. Some manual annotation will also be required.

To do so, run `bash fetch-all.sh`.

# Copying to the image

Once you are satisfied with the results, copy `bus-stops-headings.json` to
`routing/src/main/resources/` where it will be consumed during the build process and used.

# Update `distances_cache.dat.gz`

Next, you need to run  `routing-on-maps/src/main/scala/sg/beeline/DistanceMatrixCachingApp.scala` to
recreate `distances_cache.dat.gz`. **Note: this will easily take a few days on a powerful computer.**
You are advised to run it on a remote server over the weekend.

# Copy new `distances_cache.dat.gz` into the `resources`

And then build your image, and you're done!

(Sorry this process couldn't be more automated, but the processing time of 1-2+ days will lend itself
to being killed by most CI/CD environments)
