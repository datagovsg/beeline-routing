#!/bin/bash

set -eou pipefail

# OneMap no longer provides a comprehensive list of
# bus stops
# python fetch_bus_stops_from_onemap.py
python fetch_bus_stops_from_data_mall.py
python fetch_bus_routes_from_data_mall.py

# At this point, we need Graphhopper running
docker build -t sggh graphhopper
docker run --rm -d -p '8989:8989' sggh

until curl 'http://localhost:8989'
do
  sleep 1 # give Graphhopper some time to start up
done

# python combine_onemap_and_data_mall.py
python find_stop_headings.py

python migrate_bus_stops.py
