#!/usr/bin/env python

from httplib import HTTPConnection
import cPickle

conn = HTTPConnection('www.onemap.sg')

conn.request('GET', '/config.xml')
resp = conn.getresponse()
headers = resp.getheaders()
body = resp.read()

def GetURL(stuff):
    start='<%s>' % stuff
    end='</%s>' % stuff

    start_index= body.find(start)
    end_index =  body.find(end, start_index)

    url = body[start_index + len(start) : end_index]

    return url

def GetConfig():
    return body



