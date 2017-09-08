#!/bin/sh

docker run --restart always -p 5123:5000 -d beeline-routing:latest
