#!/usr/bin/env python
import data_mall
import json
import os

data_mall.authenticate(os.environ.get('DATA_MALL_API_TOKEN'))

bus_routes = data_mall.get('BusRoutes', 0)

json.dump(bus_routes, open('bus_routes_from_data_mall.json', 'w'), indent=2)
