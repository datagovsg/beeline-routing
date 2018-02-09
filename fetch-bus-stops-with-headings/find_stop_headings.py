import requests
import json
import itertools as it
import pyproj

SVY = pyproj.Proj(init='epsg:3414')


BusRoutes = json.load(open('./bus_routes_from_data_mall.json'))
# BusStops = json.load(open('./combined_bus_stops.json'))
BusStops = {
    b['BusStopCode']: b
    for b in json.load(open('./bus_stops_from_data_mall.json'))
}

def uniq_by(ls, key=lambda x: x):
    s = set()

    for item in ls:
        k = key(item)

        if k in s:
            continue
        else:
            s.add(k)
            yield item

def dedup_successive(ls):
    return [x for x,y in it.groupby(ls)]


stopsByRoute = []

for (service, direction), route_stops in it.groupby(BusRoutes, lambda x : (x['ServiceNo'], x['Direction'])):
    sorted_route_stops = sorted(
        uniq_by(route_stops, key=lambda x:x['StopSequence']),
        key=lambda x:x['StopSequence']
    )

    stopsByRoute.append(((service, direction), sorted_route_stops))

filter(lambda x:x[0][0] == '410', stopsByRoute)


# In[44]:

# for each route, emit (this stop, next stop)

# obtain the (prev, next) pairs
def prev_next(bus_stops):
    for a,b in zip(bus_stops, bus_stops[1:]):
        if a['BusStopCode'] == b['BusStopCode']:
            raise ValueError((a['BusStopCode'], bus_stops))

        yield a['BusStopCode'], b['BusStopCode']

stop_pairs = [
    (prev, next)
    for sbr in stopsByRoute
    for prev, next in prev_next(sbr[1])
]
# Exclude the non-stopping ones
stop_pairs = [x for x in stop_pairs if not x[0].startswith('E') and not x[1].startswith('E')]
stop_pairs = list(set(stop_pairs))

print(stop_pairs[1])

def make_request(pair):
    ll1 = BusStops[pair[0]]
    ll2 = BusStops[pair[1]]

    return ['%s,%s' % (ll1['Latitude'], ll1['Longitude']),
            '%s,%s' % (ll2['Latitude'], ll2['Longitude'])]


def get_route(stop_pair):
    import polyline

    res = requests.get('http://localhost:8989/route',
        params={
            "point": [
                make_request(stop_pair)
            ],
            "type": "json",
            "vehicle": "car",
            "weighting": "fastest",
        })

    res = json.loads(res.text)

    if not 'paths' in res:
        return None

    return polyline.decode(res['paths'][0]['points'])

def has_stop_location(stop_pair):
    return stop_pair[0] in BusStops         and stop_pair[1] in BusStops

pair_routes = [(get_route(stop_pair), stop_pair) for stop_pair in stop_pairs if has_stop_location(stop_pair)]
pair_routes = [x for x in pair_routes if x[0] != None]


# In[49]:

# Display a routed path

print pair_routes

import folium

m = folium.Map(location=[1.38, 103.8], zoom_start=12)
folium.PolyLine(pair_routes[100][0]).add_to(m)
m


# In[53]:

import math

def swap((x,y)):
    return (y,x)

def heading(ll1, ll2):
    xy1 = SVY(*swap(ll1))
    xy2 = SVY(*swap(ll2))

    return (xy2[0] - xy1[0], xy2[1] - xy1[1])

deduped_pair_routes = [
    (dedup_successive(path), stops) for path, stops in pair_routes if len(dedup_successive(path)) >= 2
]

beginning_headings = [
    (heading(path[0], path[1]), stops[0]) for path, stops in deduped_pair_routes
]
end_headings = [
    (heading(path[-2], path[-1]), stops[1]) for path, stops in deduped_pair_routes
]

headings = beginning_headings + end_headings


# In[58]:

stop_headings = {
    x: list([z[0] for z in y])
    for x,y in it.groupby(sorted(headings,
                                 key=lambda x:x[1]),
                          key=lambda x:x[1])
}

stop_headings['01029']


# In[59]:

multiple = [(s,x) for s,x in stop_headings.items() if len(x) > 1]


# In[62]:

def cosine_distance((a,b), (c,d)):
    return (a*c + b*d) / math.sqrt(a*a + b*b) / math.sqrt(c*c + d*d)

def cosine_distance_to_first(ls):

    return [
        cosine_distance(ls[0], l) for l in ls[1:]
    ]

multiple_headings = [ (s, cosine_distance_to_first(l)) for s,l in multiple]


# In[67]:

import numpy as np

with_discrepencies = dict([
        (s, similarities)
        for s, similarities in multiple_headings
        if np.min(similarities) < 0.9
    ])
with_discrepencies


# In[70]:

import numpy as np

# Get the final list...
def average_heading(xys):
    acc = np.array([0.0, 0.0])

    for xy in xys:
        xya = np.array(xy)
        xy_normalized = xy / np.linalg.norm(xya)

        acc += xy_normalized

    return math.atan2(*acc)

stop_average_headings = dict([
    (s, average_heading(x))
    for s, x in stop_headings.items()
    if s not in with_discrepencies
])
stop_average_headings


# In[78]:

def add_heading(d, h):
    if h is None:
        return dict(
            d.items() + [
                (u'Heading', None),
                (u'AutoHeading', False),
            ]
        )
    else:
        return dict(
            d.items() + [
                (u'Heading', h),
                (u'AutoHeading', True),
            ])

bus_stops_with_headings = [
    add_heading(s, stop_average_headings.get(s['BusStopCode']))
    for s in BusStops.values()
]
bus_stops_with_headings


# In[79]:


print 'Found headings for {} stops. Please manually find headings for remaining {} stops' \
    .format(
        len(filter(lambda x:x['Heading'] is not None, bus_stops_with_headings)),
        len(filter(lambda x:x['Heading'] is None, bus_stops_with_headings))
    )

json.dump(bus_stops_with_headings, open('bus_stops_with_headings.json', 'w'), indent=2)
