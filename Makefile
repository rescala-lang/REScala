publishLocal:
	sbtn 'rescalaAggregate / publishLocal'
	sbtn 'kofreAggregate / publishLocal'

publishJitpack:
	RESCALA_SOURCE_MAP_PREFIX="https://raw.githubusercontent.com/rescala-lang/REScala/" sbt -Dsbt.log.noformat=true 'rescalaAggregate / publishM2'
	RESCALA_SOURCE_MAP_PREFIX="https://raw.githubusercontent.com/rescala-lang/REScala/" sbt -Dsbt.log.noformat=true 'kofreAggregate / publishM2'

publishSigned:
	sbtn 'rescalaAggregate / publishSigned'
	sbtn 'kofreAggregate / publishSigned'

runSimpleCaseStudy:
	sbtn 'examples / run'

buildReplication:
	sbtn 'replicationExamplesJVM/stageJars'

runReplication: buildReplication
	java -cp "Modules/Example Replication/jvm/target/jars/*" replication.cli --help

