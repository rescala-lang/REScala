#!/bin/bash

DOCKERCMD=docker
# use podman if available
if command -v podman &> /dev/null; then
  DOCKERCMD=podman
fi

$DOCKERCMD build -t gh0st42/dtn7-showroom .
