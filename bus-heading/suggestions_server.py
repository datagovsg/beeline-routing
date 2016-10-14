#!/usr/bin/env python

## FIXME: Currently, user state is stored in memory.
## Might want to migrate it to some file.
##

import subprocess
import cPickle
import gzip
import scipy.spatial
import pyproj
import numpy as np
import itertools
import sys
import pymongo
import json
from pymongo import MongoClient

client = MongoClient()
db = client.routing


import json
data = json.load(open('../bus-stops.json'))

from datetime import timedelta
from flask import make_response, request, current_app
from functools import update_wrapper

def crossdomain(origin=None, methods=None, headers=None,
                max_age=21600, attach_to_all=True,
                automatic_options=True):
    if methods is not None:
        methods = ', '.join(sorted(x.upper() for x in methods))
    if headers is not None and not isinstance(headers, basestring):
        headers = ', '.join(x.upper() for x in headers)
    if not isinstance(origin, basestring):
        origin = ', '.join(origin)
    if isinstance(max_age, timedelta):
        max_age = max_age.total_seconds()

    def get_methods():
        if methods is not None:
            return methods

        options_resp = current_app.make_default_options_response()
        return options_resp.headers['allow']

    def decorator(f):
        def wrapped_function(*args, **kwargs):
            print request.method

            if automatic_options and request.method == 'OPTIONS':
                resp = current_app.make_default_options_response()
            else:
                resp = make_response(f(*args, **kwargs))
            if not attach_to_all and request.method != 'OPTIONS':
                return resp

            print args
            print kwargs
            h = resp.headers

            h['Access-Control-Allow-Origin'] = origin
            h['Access-Control-Allow-Methods'] = get_methods()
            h['Access-Control-Max-Age'] = str(max_age)
            if headers is not None:
                h['Access-Control-Allow-Headers'] = headers

            return resp, 200

        f.provide_automatic_options = False
        return update_wrapper(wrapped_function, f)
    return decorator


cors = crossdomain(origin='*', methods=['POST', 'GET'], attach_to_all=True,
headers=['content-type'])

from functools import wraps
from datetime import datetime
from flask import Flask, request, jsonify, session, redirect, make_response
from oauth2client.client import OAuth2WebServerFlow, FlowExchangeError
from scipy.spatial import KDTree
from bson.json_util import dumps
from bson.objectid import ObjectId

print "Process started"

app = Flask(__name__)
app.debug = True
app.secret_key = "my secure key to sign beeline analytics"

svy21 = pyproj.Proj(init='epsg:3414')

default_headers = {
    "content-type": "application/json",
}
@app.route('/next')
@cors
def get_next():
    stop = db.busStopHeading.find_one({
        "heading": {"$exists": False},
        "BusStopCode": {"$in": [u'46791',
 u'21611',
 u'68171',
 u'59619',
 u'63209',
 u'N4569',
 u'95011',
 u'14399',
 u'67361',
 u'55111',
 u'43267',
 u'43261',
 u'11379',
 u'77281',
 u'07529',
 u'16061',
 u'16249',
 u'16241',
 u'54429',
 u'64519',
 u'90079',
 u'57081',
 u'48131',
 u'83281',
 u'58331',
 u'91079',
 u'62251',
 u'24559',
 u'18151',
 u'59009',
 u'06131',
 u'16121',
 u'93129',
 u'46069',
 u'42039',
 u'52119',
 u'80119',
 u'53029',
 u'95191',
 u'97209',
 u'14419',
 u'46809',
 u'18099',
 u'41149',
 u'47651',
 u'47659',
 u'95141',
 u'63269',
 u'28659',
 u'60251',
 u'43151',
 u'46671',
 u'44699',
 u'44691',
 u'49249',
 u'54071',
 u'31009',
 u'47641',
 u'47649',
 u'11009',
 u'77129',
 u'43231',
 u'43019',
 u'21099',
 u'45401',
 u'27479',
 u'49199',
 u'10349',
 u'99171',
 u'N6709',
 u'47671',
 u'78279',
 u'04121',
 u'27459',
 u'47661',
 u'22609',
 u'22351',
 u'77251',
 u'07571',
 u'28111',
 u'96219',
 u'21079',
 u'64481',
 u'17149',
 u'64361',
 u'17091',
 u'84299',
 u'04339',
 u'18211',
 u'24349',
 u'76241',
 u'61101',
 u'65801',
 u'93159',
 u'43801',
 u'14429',
 u'45369',
 u'78149',
 u'77301',
 u'18029',
 u'56079',
 u'16099',
 u'16091',
 u'64509',
 u'17239',
 u'46409',
 u'46401',
 u'54481',
 u'65139',
 u'59611',
 u'67389',
 u'34009',
 u'75069',
 u'83301',
 u'75379',
 u'93199',
 u'22411',
 u'17201',
 u'63149',
 u'67281',
 u'72021',
 u'34101',
 u'34109',
 u'31081',
 u'31089',
 u'19049',
 u'92239',
 u'62091',
 u'27469',
 u'52049',
 u'55371',
 u'17141',
 u'44731',
 u'10589',
 u'43611',
 u'67259',
 u'51071',
 u'29149',
 u'92229',
 u'97181',
 u'52411',
 u'40051',
 u'04251',
 u'10599',
 u'41131',
 u'41139',
 u'44189',
 u'76269',
 u'93139',
 u'22331',
 u'22449',
 u'07331',
 u'25009',
 u'64479',
 u'25269',
 u'65061',
 u'58091',
 u'10121',
 u'68159',
 u'68151',
 u'N4541',
 u'62261',
 u'01229',
 u'28169',
 u'52271',
 u'27361',
 u'52499',
 u'54439',
 u'41119',
 u'14381',
 u'01109',
 u'18281',
 u'67351',
 u'54311',
 u'96101',
 u'96201',
 u'95131']},
        "Longitude": {"$ne": 0}
    })

    return dumps(stop), 200, default_headers

@app.route('/put', methods=['POST', 'OPTIONS'])
@cors
def put_one():

    print request.json.get('heading')
    print request.json.get('_id')['$oid']
    print db.busStopHeading.find_one({
        "_id": ObjectId(request.json.get('_id')['$oid'])
    })
    stop = db.busStopHeading.find_one_and_update({
        "_id": ObjectId(request.json.get('_id')['$oid'])
    }, {
        "$set": {"heading": request.json.get('heading')}
    })

    return "{}", 200, default_headers


app.run(port=9999)
