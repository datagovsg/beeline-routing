#!/usr/bin/env python
import data_mall
import json
import os

data_mall.authenticate(os.environ['DATA_MALL_API_TOKEN'])

bus_stops = data_mall.get('BusStops', 0)

json.dump(bus_stops, open('bus_stops_from_data_mall.json', 'w'), indent=2)
