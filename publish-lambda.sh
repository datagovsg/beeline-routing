#!/bin/bash
set -euo pipefail

sbt routingLambda/assembly
aws s3 cp ./routing-lambda/target/scala-2.12/routing-lambda-assembly-1.0.jar s3://beeline-analytics/
aws lambda update-function-code --function-name beeline-routing-suggestions --s3-bucket beeline-analytics --s3-key routing-lambda-assembly-1.0.jar

