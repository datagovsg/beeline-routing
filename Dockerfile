FROM openjdk:jre-alpine

WORKDIR /app

COPY ./SG-gh /app/SG-gh
COPY ./onemap/bus-stops-headings.json /app/onemap/bus-stops-headings.json
COPY ./config.properties /app

COPY ./target/scala-2.11/beeline-routing-assembly*.jar /app/beeline-routing.jar
# COPY ./distances_cache.dat.gz /app


ENV PORT 5000

RUN java -jar /app/beeline-routing.jar cache
CMD java -jar /app/beeline-routing.jar

