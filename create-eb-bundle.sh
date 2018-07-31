#!/bin/bash

set -eou pipefail

# Ensure file exists
JAR_FILE=./routing/target/scala-2.12/routing-assembly-*.jar
[ -f $JAR_FILE ]

# Download the SG map
curl https://download.geofabrik.de/asia/malaysia-singapore-brunei-latest.osm.pbf -o SG.pbf

# Activate the map
java -jar $JAR_FILE initialize-geo

# Bundle everything into one file
mv $JAR_FILE beeline-routing.jar

zip bundle.zip beeline-routing.jar
zip bundle.zip SG.pbf
zip bundle.zip SG-gh
zip bundle.zip distances_cache.dat.gz
zip bundle.zip Procfile

zip bundle.zip onemap/bus-stops-headings.json
zip bundle.zip onemap/mrt-stations.json /app
zip bundle.zip ./config.properties
