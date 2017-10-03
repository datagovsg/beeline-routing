FROM java:8-jre


WORKDIR /gh

# Use wget for caching
RUN wget https://oss.sonatype.org/content/groups/public/com/graphhopper/graphhopper-web/0.7-SNAPSHOT/graphhopper-web-0.7-20160603.180139-71-with-dep.jar -O /gh/graphhopper.jar
RUN wget https://download.geofabrik.de/asia/malaysia-singapore-brunei-latest.osm.pbf -O /gh/SG-gh.pbf

RUN /bin/echo -e """\
graph.flag_encoders=car|turn_costs=true\n\
prepare.ch.weightings=no\n\
graph.dataaccess=RAM_STORE\n\
""" > config.properties

EXPOSE 8989

CMD "java" "-jar" "/gh/graphhopper.jar" jetty.resourcebase=webapp config=config.properties osmreader.osm=/gh/SG-gh.pbf
