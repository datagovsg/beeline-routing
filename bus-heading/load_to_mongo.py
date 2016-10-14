from pymongo import MongoClient

client = MongoClient()
db = client.routing


import json

data = json.load(open('../bus-stops.json'))

for d in data:
    db.busStopHeading.insert_one(d)


