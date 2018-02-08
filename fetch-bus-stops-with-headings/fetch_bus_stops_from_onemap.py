#!/usr/bin/env python

import urllib
import json
import gzip
import requests

data = []

# fetch the public token from OneMap
r = requests.get('https://developers.onemap.sg/publicapi/publicsessionid')
token = r.json()['access_token']

for start in range(0, 100):
    print 'Fetching bus stops starting with {0:02d}'.format(start)

    rset = 1

    while True:
        req = requests.get('https://developers.onemap.sg/commonapi/search?' + urllib.urlencode({
            'searchVal': '{0:02d} BUS STOP'.format(start),
            'pageNum': rset,
            'returnGeom': 'Y',
            'getAddrDetails': 'Y',
        }))

        obj = req.json()
        data.append(obj)

        if len(obj['results']) == 0:
            break

        rset += 1

json.dump([
    result
    for result_set in data
    for result in result_set['results']
], open('bus_stops_from_onemap.json', 'w'), indent=2)
