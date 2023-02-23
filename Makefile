publishLocal:
	sbtn 'rescalaAggregate / publishLocal'
	sbtn 'kofreAggregate / publishLocal'

publishJitpack:
	for sv in 2.11 2.12 2.13 3; do SCALA_VERSION=$$sv sbt -Dsbt.log.noformat=true 'rescalaAggregate / publishM2'; done
	sbt -Dsbt.log.noformat=true 'kofreAggregate / publishM2'


publishSigned:
	sbtn 'rescalaAggregate / publishSigned'
	sbtn 'kofreAggregate / publishSigned'

runSimpleCaseStudy:
	sbtn 'examples / run'

buildReplication:
	sbtn 'replicationExamplesJVM/stageJars'

runReplication: buildReplication
	java -cp "Code/Examples/Replication/jvm/target/jars/*" replication.cli --help

