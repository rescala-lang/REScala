.ONESHELL:
SHELL = /usr/bin/bash

publishLocal:
	sbtn '+ rescalaCore / publishLocal'
	sbtn '+ kofreJS / publishLocal'
	sbtn '+ kofreJVM / publishLocal'
	sbtn '+ kofreNative / publishLocal'

publishSigned:
	sbtn '+ rescalaCore / publishSigned'
	sbtn '+ kofreJS / publishSigned'
	sbtn '+ kofreJVM / publishSigned'
	sbtn '+ kofreNative / publishSigned'

runSimpleCaseStudy:
	sbtn 'examples / run'

buildReplication:
	sbtn 'replicationExamplesJVM/stageJars'

runReplication: buildReplication
	java -cp "Code/Examples/Replication/jvm/target/jars/*" replication.cli --help

