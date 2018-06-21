# Compile from source to Scala
FROM hseeberger/scala-sbt:8u151-2.12.4-1.0.2 AS compile

WORKDIR /build

# Hack to fetch sbt first. Makes future compilation faster
COPY ./project/assembly.sbt /build/project/assembly.sbt
COPY ./project/plugins.sbt /build/project/plugins.sbt
COPY ./project/build.properties /build/project/build.properties
RUN touch /build/build.sbt
RUN sbt assembly

# Fetch the rest of our dependencies
COPY ./build.sbt /build/build.sbt
RUN sbt assembly

# COPY ./scalastyle-config.xml /build/
COPY ./routing /build/routing
COPY ./fuzzy-clustering /build/fuzzy-clustering

RUN sbt routing/assembly

# Run the assembly, generate the intermediate files (cache, maps)
FROM openjdk:jre-alpine AS one

WORKDIR /app

COPY ./onemap/bus-stops-headings.json /app/onemap/bus-stops-headings.json
COPY ./onemap/mrt-stations.json /app
COPY ./config.properties /app

COPY --from=compile /build/routing/target/scala-2.11/routing-assembly-*.jar /app/beeline-routing.jar

# Build the SG map cache files
RUN apk add --no-cache curl && \
	curl https://download.geofabrik.de/asia/malaysia-singapore-brunei-latest.osm.pbf -o /app/SG.pbf && \
	apk del curl
RUN java -jar /app/beeline-routing.jar initialize-geo

# Rebuild the distance cache if requested
COPY ./distances_cache.dat.gz /app/
ARG SKIP_CACHE_UPDATE=1
RUN if [ "${SKIP_CACHE_UPDATE}" = "" ]; then java -jar /app/beeline-routing.jar cache; fi

# Final docker image
FROM openjdk:jre-alpine AS final

WORKDIR /app

COPY --from=one /app/SG-gh /app/SG-gh
COPY --from=one /app/distances_cache.dat.gz /app

COPY ./onemap/bus-stops-headings.json /app/onemap/bus-stops-headings.json
COPY ./onemap/mrt-stations.json /app
COPY ./config.properties /app

COPY --from=compile /build/routing/target/scala-2.11/routing-assembly-*.jar /app/beeline-routing.jar

ENV PORT 5000

CMD java -jar /app/beeline-routing.jar

