#!/usr/bin/fish

set -l curr (pwd)
cd ../../..
sbtn "replicationExamplesJS/deploy; replicationExamplesJVM/stageJars"
cd $curr
java --class-path "jvm/target/jars/*" replication.cli conn --webserver-listen-port 3004 --webserver-static-path js/target/scala-3.2.2/replicationexamples-fastopt
