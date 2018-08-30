#!/bin/bash

set -eou pipefail

rm $(find . -path '*/target/*.jar') || true

sbt routing/assembly
sbt routingWeb/assembly

ls routing/target/scala-2.12
ls routing-web/target/scala-2.12

# Ensure file exists
ROUTING_JAR_FILE=./routing/target/scala-2.12/routing-assembly-1.0.jar
WEB_JAR_FILE=./routing-web/target/scala-2.12/routing-web-assembly-1.0.jar
[ -f $ROUTING_JAR_FILE ] || (echo "$ROUTING_JAR_FILE does not exist" && false)
[ -f $WEB_JAR_FILE ] || (echo "$WEB_JAR_FILE does not exist" && false)

# Download the SG map
curl https://download.geofabrik.de/asia/malaysia-singapore-brunei-latest.osm.pbf -o SG.pbf

# Activate the map
java -jar $ROUTING_JAR_FILE initialize-geo

# Bundle everything into one file
mv $WEB_JAR_FILE beeline-routing.jar

zip bundle.zip beeline-routing.jar
zip bundle.zip SG.pbf
zip bundle.zip SG-gh
zip bundle.zip Procfile

zip bundle.zip ./config.properties
