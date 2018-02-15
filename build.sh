#!/bin/bash

set -eou pipefail

sbt 'set test in assembly := {}' clean assembly
# rm -r SG-gh
# tar xfz SG-gh.ghz
java -jar ./target/scala-2.11/beeline-routing-assembly*.jar cache

docker build -t beeline-routing .


