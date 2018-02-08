#!/usr/bin/env python
"""
Migrates from an older list of bus stops to a newer list of bus stops.

Reason for this is that we must maintain the index
"""


# coding: utf-8

# In[39]:

import json

def lat_lng_distance(ll1, ll2):
    import math

    radians = lambda degree: degree / 180 * math.pi
    RADIUS = 6371e3

    D_LNG = (ll1[1] - ll2[1])
    D_LAT = (ll1[0] - ll2[0])
    SUM_LAT = (ll1[0] - ll2[0])

    D_X = radians(D_LNG) * math.cos(radians(SUM_LAT) / 2)
    D_Y = radians(D_LAT)

    return RADIUS * math.sqrt(
        D_Y * D_Y + D_X * D_X
    )

class Migration:
    def __init__(self, old_list, new_list):
        self.old_list = old_list
        self.new_list = new_list

    def get(self, tup):
        if tup[0] == 'O':
            return self.old_list[tup[1]]
        elif tup[0] == 'N':
            return self.new_list[tup[1]]
        else:
            assert False

    def distance(self, s1i, s2i):
        s1 = self.get(s1i)
        s2 = self.get(s2i)

        distance = lat_lng_distance(
            (float(s1['Latitude']), float(s1['Longitude'])),
            (float(s2['Latitude']), float(s2['Longitude'])),
        )

        # Short circuit if they use the same bus stop code? :rolleyes:
        # Note the negative number -- negative distance means this
        # is always prioritized
        if s1['BusStopCode'] == s2['BusStopCode'] and distance <= 20:
            return -0.01
        else:
            return distance

if __name__ == '__main__':
# In[41]:
    old_list = json.load(open('../onemap/bus-stops-headings.json'))
    new_list = json.load(open('./bus_stops_with_headings.json'))
    
    old_list_indices = [('O', i) for i in range(len(old_list))]
    new_list_indices = [('N', i) for i in range(len(new_list))]
    
    m = Migration(old_list, new_list)
    
    # Greedy matching of bus stops
    # Yes, I'm using O(n) algorithms where O(lg n) ones exist. But this is
    # purely for readability reasons.
    matches = []
    for oi in old_list_indices:
        if m.get(oi).get('Defunct', False):
            continue
    
        for ni in new_list_indices:
            dist = m.distance(oi, ni)
            if dist < 20:
                matches.append((dist, oi, ni))
    
    
    # In[43]:
    
    matches.sort()
    
    # Don't allow a match to be used more than once -- so we track the usage
    used_indices = set([])
    final_matches = []
    
    for dist, oi, ni in matches:
        if oi not in used_indices and ni not in used_indices:
            used_indices.add(oi)
            used_indices.add(ni)
            final_matches.append((oi, ni))
    
    
    # In[44]:
    
    # Build the new bus stops list:
    # 1) old stops -- if a match exists, use it. else mark as defunct
    # 2) new stops -- append at the end
    final_matches_dict = dict(final_matches)
    final_list = list()
    
    for oi in old_list_indices:
        if oi in final_matches_dict:
            final_list.append(m.get(final_matches_dict[oi]))
        else:
            odefunct = m.get(oi).copy()
            odefunct['Defunct'] = True
    
            final_list.append(odefunct)
    
    for ni in new_list_indices:
        if ni not in used_indices:
            final_list.append(m.get(ni))
    
    
    # In[47]:
    
    # Sanity check:
    
    NUM_DEFUNCT_BEFORE = len(filter(lambda x: x.get('Defunct', False), old_list))
    NUM_DEFUNCT_AFTER = len(filter(lambda x: x.get('Defunct', False), final_list))
    
    print("DEFUNCT BEFORE: {}".format(NUM_DEFUNCT_BEFORE))
    print("DEFUNCT AFTER: {}".format(NUM_DEFUNCT_AFTER))
    print("{}".format(len(final_matches)))
    print("LIST BEFORE: {}".format(len(old_list)))
    print("LIST NEW: {}".format(len(new_list)))
    print("LIST AFTER: {}".format(len(final_list)))
    
    assert len(final_list) == (
        len(new_list) + NUM_DEFUNCT_AFTER + NUM_DEFUNCT_BEFORE
    )
    
    
    # In[52]:
    
    for old, new in zip(old_list, final_list):
        if not old.get('Defunct', False) and new.get('Defunct', False):
            # If a stop is newly defunct, then ensure that the
            # new stop is exactly equal to the old stop
            assert new['Latitude'] == old['Latitude']
            assert new['Longitude'] == old['Longitude']
        
        elif old.get('Defunct', False) and new.get('Defunct', False):
            # If both are defunct, then also ensure exactly equal
            assert new['Latitude'] == old['Latitude']
            assert new['Longitude'] == old['Longitude']
        
        elif old.get('Defunct', False) and not new.get('Defunct', False):
            assert False, "Stop should not change from defunct to not-defunct"
        
        elif not old.get('Defunct', False) and not new.get('Defunct', False):
            assert lat_lng_distance(
                [old['Latitude'], old['Longitude']],
                [new['Latitude'], new['Longitude']],
            ) < 20
    
    
    # In[53]:
    
    json.dump(final_list, open('./migrated_bus_stops.json', 'w'), indent=2)
    
