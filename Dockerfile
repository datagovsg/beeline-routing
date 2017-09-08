FROM openjdk:jre-alpine

WORKDIR /app
COPY ./target/scala-2.11/beeline-routing-assembly*.jar /app/beeline-routing.jar
COPY ./SG-gh /app/SG-gh
COPY ./config.properties /app
COPY ./distances_cache.dat.gz /app

COPY ./onemap/bus-stops-headings.json /app/onemap/bus-stops-headings.json

ENV PORT 5000

CMD java -jar /app/beeline-routing.jar

