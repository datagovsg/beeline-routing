#!/bin/bash

set -eou pipefail


(docker stop beeline-routing && docker rm beeline-routing) || echo 'beeline-routing not started'

docker run --name beeline-routing --env "DATABASE_URL=$DATABASE_URL" --restart always -p 5123:5000 -d beeline-routing:latest
