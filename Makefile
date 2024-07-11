publishLocal:
	sbt --client 'rdtsAggregate / publishLocal'
	sbt --client 'reactivesAggregate / publishLocal'

publishJitpack:
	CUSTOM_SCALAJS_SOURCE_MAP_PREFIX="https://raw.githubusercontent.com/rescala-lang/REScala/" sbt -Dsbt.log.noformat=true 'rdtsAggregate / publishM2'
	CUSTOM_SCALAJS_SOURCE_MAP_PREFIX="https://raw.githubusercontent.com/rescala-lang/REScala/" sbt -Dsbt.log.noformat=true 'reactivesAggregate / publishM2'

publishSigned:
	sbt --client 'rdtsAggregate / publishSigned'
	sbt --client 'reactivesAggregate / publishSigned'

runSimpleCaseStudy:
	sbt --client 'examples / run'

buildReplication:
	sbt --client 'replicationExamplesJVM/stageJars'

runReplication: buildReplication
	java -cp "Modules/Example Replication/jvm/target/jars/*" replication.cli --help

buildTodoMVC:
	sbt --client --no-color 'print todolist/deploy'

webviewExample: buildTodoMVC
	sbt --client --no-color 'webview / fetchResources'
	sbt --client --no-color 'webview / run Modules/Examples/TodoMVC/target/index.html'
