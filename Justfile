# https://github.com/casey/just

test sbtOpts="":
	sbt {{sbtOpts}} test

publishLocal sbtOpts="":
	sbt {{sbtOpts}} 'rdtsAggregate / publishLocal'
	sbt {{sbtOpts}} 'reactivesAggregate / publishLocal'


# most useful for teh jitpack export, though not sure if that even works …
export CUSTOM_SCALAJS_SOURCE_MAP_PREFIX:="https://raw.githubusercontent.com/rescala-lang/REScala/"

# supposed to be used to publish when running on jitpack
publishJitpack:
	sbt -Dsbt.log.noformat=true 'rdtsAggregate / publishM2'
	sbt -Dsbt.log.noformat=true 'reactivesAggregate / publishM2'

publishSigned sbtOpts="":
	sbt {{sbtOpts}} 'rdtsAggregate / publishSigned'
	sbt {{sbtOpts}} 'reactivesAggregate / publishSigned'

runSimpleCaseStudy sbtOpts="":
	sbt {{sbtOpts}} 'examplesMiscJVM / run'

buildReplication sbtOpts="":
	sbt {{sbtOpts}} 'replicationExamplesJVM/packageJars'

runReplication:
	#!/usr/bin/fish
	set -l path (sbt -error 'print replicationExamplesJVM/packageJars')
	java -cp $path"/*" replication.cli --help

buildTodoMVC sbtOpts="":
	sbt {{sbtOpts}} 'print todolist/deploy'

webviewExample sbtOpts="": (buildTodoMVC sbtOpts)
	sbt {{sbtOpts}} 'webview / fetchResources'
	sbt {{sbtOpts}} 'webview / run Modules/Examples/TodoMVC/target/index.html'

selectScheduler scheduler="levelled":
	scala-cli --jvm=system --server=false scripts/select-scheduler.scala -- {{scheduler}}
