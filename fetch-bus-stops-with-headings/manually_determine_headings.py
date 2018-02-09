from flask import Flask, jsonify
import hashlib
import os
import sys

from flask import request
from werkzeug.utils import redirect


def initialize_data_file():
    import json
    # The original file is at migrate_bus_stops.json
    # Create the file with headings at with-headings-<hash of original>.json
    H = hashlib.new('md5')

    orig_filename = './migrated_bus_stops.json'
    orig_data = json.load(open(orig_filename))

    with open(orig_filename, 'rb') as f:
        buf = f.read(4096)
        H.update(buf)

    dig = H.hexdigest()
    dest_filename = 'with-headings-{}.json'.format(dig)

    if os.path.isfile(dest_filename):
        sys.stderr.write('{} already exists.'.format(dest_filename))

        # Sanity check
        saved_data = json.load(open(dest_filename))
        assert len(saved_data) == len(orig_data), "Saved data seems to be corrupt"

        return dest_filename, saved_data
    else:
        sys.stderr.write('You will be writing to {}'.format(dest_filename))

        return dest_filename, orig_data


def create_app(data_filename, data):
    app = Flask('heya')

    @app.route('/')
    def index():
        return redirect('/static/index.html')

    @app.route('/items', methods=['GET'])
    def get_items():
        import json
        print(json.dumps(data[0], indent=2))
        return jsonify(data)

    @app.route('/items/<int:index>', methods=['PUT'])
    def put_item(index):
        assert index < len(data)

        # Some basic validation
        payload = request.json
        assert isinstance(payload['Heading'], (float, int))

        data[index] = data[index].copy()
        data[index]['Heading'] = payload['Heading']

        save_current()

        return jsonify(data[index])

    def save_current():
        import json

        json.dump(data, open(data_filename, 'w'), indent=2)

    app.run('0.0.0.0', 5678, True)



if __name__ == '__main__':
    data_filename, data = initialize_data_file()

    create_app(data_filename, data)

