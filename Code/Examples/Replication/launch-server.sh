#!/usr/bin/sh

curr=$(pwd)
cd ../../..
sbtn "replicationExamplesJS/deploy; replicationExamplesJVM/stageJars"
cd $curr
java --class-path "jvm/target/jars/*" replication.cli conn --random-data-time 1000 --webserver-listen-port 3004 --webserver-static-path js/target/scala-3.2.2/replicationexamples-fastopt
