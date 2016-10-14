#!/usr/bin/env python

import onemap
import urllib
import httplib2
import json
import gzip
import cPickle

url = onemap.GetURL('RestServiceUrl')

data = []

for start in range(0,100):
    rset = 1

    while True:
        #print urllib.quote_plus('SEARCHVAL LIKE \'$BUS STOP$\' or SEARCHVAL LIKE \'BUS STOP$\' or SEARCHVAL LIKE \'$BUS STOP\'')

        rurl = url + \
            urllib.quote_plus('SEARCHVAL LIKE \'' + '{0:02d}'.format(start) + '$BUS STOP$\'') + \
        '&' + urllib.urlencode({
            'rset': str(rset),
            'otptFlds': 'POSTALCODE,CATEGORY',
        })
        print rurl

        h = httplib2.Http('.cache')
        _,content = h.request(rurl)

        print content

        obj = json.loads(content)
        data.append(obj)

        if len(obj['SearchResults']) == 1:
            break

        print len(obj['SearchResults'])

        cPickle.dump(data, gzip.open('busstops2.pickle.gz', 'w'))
        rset += 1

