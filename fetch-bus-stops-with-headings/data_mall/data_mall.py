#!/usr/bin/env python

import json
import requests

credentials = {
    "api_token": None,
    "uuid": None
}

def authenticate(api_token):
    credentials['api_token'] = api_token

def get(service, start = 0, end=None):
    return_value = []

    while end == None or start < end:
        print  '/ltaodataservice/%s?$skip=%i' % (service, start)
        r = requests.get('http://datamall2.mytransport.sg/ltaodataservice/%s?$skip=%i' % (service, start),
            headers = {
                'AccountKey': credentials['api_token'],
                # 'UniqueUserId': credentials['uuid'],
                'Accept': 'application/json'
            })

        data = r.json()

        return_value += data['value']

        if len(data['value']) == 0:
            break

        start += len(data['value'])

    return return_value
