#!/usr/bin/sh

curr=$(pwd)
cd ../..
sbt --client "replicationExamplesJS/deploy; replicationExamplesJVM/stageJars"
cd "$curr"
java --class-path "jvm/target/jars/*" replication.cli conn --webserver-listen-port 3004 --webserver-static-path js/target/scala-3.4.1/replicationexamples-fastopt --northwind-path "northwind.db" --tcp-listen-port 3005
