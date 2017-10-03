#!/usr/bin/env python

import gzip
import json
import re

onemap_stops = json.load(open('bus_stops_from_onemap.json'))
datamall_stops = json.load(open('bus_stops_from_data_mall.json'))


onemap_re = re.compile('([0-9]{5}) \(BUS STOP\)')

def keyof_onemap(s):
    m = onemap_re.match(s['BUILDING'])
    if m is not None:
        return m.group(1)

def keyof_datamall(s):
    return s['BusStopCode']

def valueof_onemap(s):
    # Onemap data has fewer details about stops, but more stops
    return {
        'Latitude': s['LATITUDE'],
        'Longitude': s['LONGITUDE'],
        'BusStopCode': keyof_onemap(s),
        'RoadName': s['ROAD_NAME'],
        'Description': '??',
        'Source': 'OneMap'
    }

def valueof_datamall(s):
    s['Source'] = 'LTA'
    return s

onemap_stops_by_stop = [
    (keyof_onemap(r), valueof_onemap(r))
    for r in onemap_stops
]

datamall_stops_by_stop = [
    (keyof_datamall(r), valueof_datamall(r))
    for r in datamall_stops
]

combined = dict(
    onemap_stops_by_stop + datamall_stops_by_stop
)

print "Combining {} from Data Mall and {} from OneMap into {} bus stops" \
    .format(len(datamall_stops_by_stop),
            len(onemap_stops_by_stop),
            len(combined))

json.dump(
    dict(
        onemap_stops_by_stop + datamall_stops_by_stop
    ),
    open('combined_bus_stops.json', 'w'),
    indent=2, sort_keys=True
)
