#!/bin/bash

set -eou pipefail

sbt routing/assembly
sbt routingWeb/assembly

# Ensure file exists
ROUTING_JAR_FILE=./routing/target/scala-2.12/routing-assembly-*.jar
WEB_JAR_FILE=./routing-web/target/scala-2.12/routing-web-assembly-*.jar
[ -f $ROUTING_JAR_FILE ]
[ -f $WEB_JAR_FILE ]

# Download the SG map
curl https://download.geofabrik.de/asia/malaysia-singapore-brunei-latest.osm.pbf -o SG.pbf

# Activate the map
java -jar $ROUTING_JAR_FILE initialize-geo

# Bundle everything into one file
mv $WEB_JAR_FILE beeline-routing.jar

zip bundle.zip beeline-routing.jar
zip bundle.zip SG.pbf
zip bundle.zip SG-gh
zip bundle.zip distances_cache.dat.gz
zip bundle.zip Procfile

zip bundle.zip onemap/bus-stops-headings.json
# zip bundle.zip onemap/mrt-stations.json /app
zip bundle.zip ./config.properties
