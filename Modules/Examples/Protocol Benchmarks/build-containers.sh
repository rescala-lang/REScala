#!/usr/bin/env bash

(
	cd ../../..
	#sbt "clean; proBench/Universal/packageBin"
	sbt "proBench/Universal/packageBin"
)

docker build -f Dockerfile -t probench_app --progress plain .
