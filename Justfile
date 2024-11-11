# https://github.com/casey/just

readme:
	pager README.md

authors:
	git shortlog -sn

test sbtOpts="":
	sbt {{sbtOpts}} test

publishLocal sbtOpts="":
	sbt {{sbtOpts}} 'rdtsAggregate / publishM2'
	sbt {{sbtOpts}} 'reactivesAggregate / publishM2'


# most useful for the jitpack export, though not sure if that even works …
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

protoBenchNativeImage:
	#!/usr/bin/env fish

	set -l jarspath (sbt --error "print proBench/packageJars")

	native-image --class-path "$jarspath/*" probench.cli probench

protoBenchRunNativeImage node="node" client="client" args="bench-1-1":
	#!/usr/bin/env fish

	kitty ./probench {{node}} --name NODE1 --listen-client-port 8010 --listen-peer-port 8011 --cluster --initial-cluster-ids NODE1 NODE2 NODE3 &
	sleep 1;
	kitty ./probench {{node}} --name NODE2 --listen-client-port 8020 --listen-peer-port 8021 --cluster localhost:8011 --initial-cluster-ids NODE1 NODE2 NODE3 &
	sleep 1;
	kitty ./probench {{node}} --name NODE3 --listen-client-port 8030 --listen-peer-port 8031 --cluster localhost:8011 localhost:8021 --initial-cluster-ids NODE1 NODE2 NODE3 &

	sleep 1;
	cat "Modules/Examples/Protocol Benchmarks/args/bench-1-1" | ./probench {{client}} --node localhost:8010 --name Client1

runProtoBench node="node" client="client" args="bench-1-1":
	#!/usr/bin/env fish

	set -l jarspath (sbt --error "print proBench/packageJars")

	kitty java --class-path "$jarspath/*" probench.cli {{node}} --name NODE1 --listen-client-port 8010 --listen-peer-port 8011 --cluster --initial-cluster-ids NODE1 NODE2 NODE3 &
	sleep 1;
	kitty java --class-path "$jarspath/*" probench.cli {{node}} --name NODE2 --listen-client-port 8020 --listen-peer-port 8021 --cluster localhost:8011 --initial-cluster-ids NODE1 NODE2 NODE3 &
	sleep 1;
	kitty java --class-path "$jarspath/*" probench.cli {{node}} --name NODE3 --listen-client-port 8030 --listen-peer-port 8031 --cluster localhost:8011 localhost:8021 --initial-cluster-ids NODE1 NODE2 NODE3 &

	sleep 1;
	cat "Modules/Examples/Protocol Benchmarks/args/bench-1-1" | java --class-path "$jarspath/*" probench.cli {{client}} --node localhost:8010 --name Client1
	# java --class-path "$jarspath/*" probench.cli client --node localhost:8010 --name Client2
	# java --class-path "$jarspath/*" probench.cli client --node localhost:8010 --name Client3

	trap 'kill $(jobs -p)' EXIT

runProtoBenchCluster node="node":
	#!/usr/bin/env fish

	set -l jarspath (sbt --error "print proBench/packageJars")

	kitty java --class-path "$jarspath/*" probench.cli {{node}} --name NODE1 --listen-client-port 8010 --listen-peer-port 8011 --cluster --initial-cluster-ids NODE1 NODE2 NODE3 &
	sleep 1;
	kitty java --class-path "$jarspath/*" probench.cli {{node}} --name NODE2 --listen-client-port 8020 --listen-peer-port 8021 --cluster localhost:8011 --initial-cluster-ids NODE1 NODE2 NODE3 &
	sleep 1;
	kitty java --class-path "$jarspath/*" probench.cli {{node}} --name NODE3 --listen-client-port 8030 --listen-peer-port 8031 --cluster localhost:8011 localhost:8021 --initial-cluster-ids NODE1 NODE2 NODE3 &

	trap 'kill $(jobs -p)' SIGINT
	sleep $last_pid

runProtoBenchClients runId client="client" benchmark="put-100k-1C1N" benchResultsDir="bench-results/":
	#!/usr/bin/env fish

	set -l jarspath (sbt --error "print proBench/packageJars")

	cd "Modules/Examples/Protocol Benchmarks"

	set -lx BENCH_RESULTS_DIR {{benchResultsDir}}
	set -lx RUN_ID {{runId}}

	for clientArgs in $(cat ./benchmarks/{{benchmark}})
		set -l args (string split ';' $clientArgs)

		cat args/$args[3] | java --class-path "$jarspath/*" probench.cli {{client}} --node $args[2] --name $args[1] &
	end

	trap 'kill $(jobs -p)' SIGINT
	sleep $last_pid


