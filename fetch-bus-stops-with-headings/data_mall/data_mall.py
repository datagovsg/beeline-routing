#!/usr/bin/env python

from httplib import HTTPConnection
import json

known_services = [
'BusStops'
]

credentials = {
    "api_token": None,
    "uuid": None
}

def authenticate(api_token, uuid):
    credentials['api_token'] = api_token
    credentials['uuid'] = uuid

def get(service, start = 0, end=None):
    conn = HTTPConnection('datamall2.mytransport.sg')

    return_value = []

    while end == None or start < end:
        print  '/ltaodataservice/%s?$skip=%i' % (service, start)
        conn.request('GET', '/ltaodataservice/%s?$skip=%i' % (service, start),
            headers = {
                'AccountKey': credentials['api_token'],
                'UniqueUserId': credentials['uuid'],
                'Accept': 'application/json'
            })

        resp = conn.getresponse()
        headers = resp.getheaders()
        body = resp.read()

        data = json.loads(body)

        return_value += data['value']

        if len(data['value']) == 0:
            break

        start += 50

    return return_value
