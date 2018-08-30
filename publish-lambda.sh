#!/bin/bash
set -euo pipefail

create_lambda () {
  aws lambda create-function --function-name beeline-routing-suggestions \
        --runtime java8 \
        --role arn:aws:iam::882000534153:role/service-role/beeline-routing \
        --handler 'sg.beeline.lambda.SuggestRouteHandler::handle' \
        --code '{"S3Bucket":"beeline-routing","S3Key":"routing-lambda-assembly-1.0.jar"}' \
        --timeout 60 \
        --memory-size 1024
}

sbt routingLambda/assembly
aws s3 cp ./routing-lambda/target/scala-2.12/routing-lambda-assembly-1.0.jar s3://beeline-routing/

create_lambda || true
aws lambda update-function-code --function-name beeline-routing-suggestions --s3-bucket beeline-routing --s3-key routing-lambda-assembly-1.0.jar

