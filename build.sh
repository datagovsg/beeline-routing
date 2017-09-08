#!/bin/bash

set -eou pipefail

sbt 'set test in assembly := {}' clean assembly
rm -r SG-gh
tar xfz SG-gh.ghz
docker build -t beeline-routing .


