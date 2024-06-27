#!/bin/bash

if [ -z "$1" ]; then
  echo "Usage: $0 <version tag>"
  exit 1
fi

TAG=$1

if command -v podman &> /dev/null; then
  podman build --platform linux/amd64,linux/arm64 --manifest gh0st42/dtn7-showroom .
  podman push localhost/gh0st42/dtn7-showroom:latest docker://docker.io/gh0st42/dtn7-showroom:$TAG
  podman push localhost/gh0st42/dtn7-showroom:latest docker://docker.io/gh0st42/dtn7-showroom:latest
else
  docker buildx build --platform linux/amd64,linux/arm64 -t gh0st42/dtn7-showroom:$TAG --push .
  docker buildx build --platform linux/amd64,linux/arm64 -t gh0st42/dtn7-showroom:latest --push .
fi