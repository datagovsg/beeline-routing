#!/bin/bash

set -eou pipefail

python fetch_bus_stops_from_onemap.py
python fetch_bus_stops_from_data_mall.py

python combine_onemap_and_data_mall.py
python find_stop_headings.py
