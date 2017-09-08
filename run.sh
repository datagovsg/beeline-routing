#!/bin/sh

docker run --env "DATABASE_URL=$DATABASE_URL" --restart always -p 5123:5000 -d beeline-routing:latest
