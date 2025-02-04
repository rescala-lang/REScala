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

	sbt --client 'set proBench/JarExport.packageJarsPath := "target/probench/jars"; proBench/packageJars'

	set -l graalvmPath (cs java-home --jvm graalvm-java23:23.0.2)
	$graalvmPath/bin/native-image --class-path "target/probench/jars/*" probench.cli probench

protoBenchRunNativeImage node="node" client="client" args="bench-1-1":
	#!/usr/bin/env fish

	kitty ./probench {{node}} --name NODE1 --listen-client-port 8010 --listen-peer-port 8011 --cluster --initial-cluster-ids NODE1 NODE2 NODE3 &
	sleep 1;
	kitty ./probench {{node}} --name NODE2 --listen-client-port 8020 --listen-peer-port 8021 --cluster localhost:8011 --initial-cluster-ids NODE1 NODE2 NODE3 &
	sleep 1;
	kitty ./probench {{node}} --name NODE3 --listen-client-port 8030 --listen-peer-port 8031 --cluster localhost:8011 localhost:8021 --initial-cluster-ids NODE1 NODE2 NODE3 &

	sleep 1;
	cat "Modules/Examples/Protocol Benchmarks/args/bench-1-1" | ./probench {{client}} --node localhost:8010 --name Client1

runProtoBenchNoNet args="bench-1-1":
	sbt --client 'set proBench/JarExport.packageJarsPath := "target/probench/jars"; proBench/packageJars'
	cat "Modules/Examples/Protocol Benchmarks/args/bench-1-1" | java --class-path "target/probench/jars/*" probench.cli easy-setup

runProtoBench node="node" client="client" args="bench-1-1":
	#!/usr/bin/env fish

	sbt --client 'set proBench/JarExport.packageJarsPath := "target/probench/jars"; proBench/packageJars'

	kitty java --class-path "target/probench/jars/*" probench.cli {{node}} --name NODE1 --listen-client-port 8010 --listen-peer-port 8011 --cluster --initial-cluster-ids NODE1 NODE2 NODE3 &
	sleep 1;
	kitty java --class-path "target/probench/jars/*" probench.cli {{node}} --name NODE2 --listen-client-port 8020 --listen-peer-port 8021 --cluster localhost:8011 --initial-cluster-ids NODE1 NODE2 NODE3 &
	sleep 1;
	kitty java --class-path "target/probench/jars/*" probench.cli {{node}} --name NODE3 --listen-client-port 8030 --listen-peer-port 8031 --cluster localhost:8011 localhost:8021 --initial-cluster-ids NODE1 NODE2 NODE3 &

	sleep 1;
	cat "Modules/Examples/Protocol Benchmarks/args/{{args}}" | java --class-path "target/probench/jars/*" probench.cli {{client}} --node localhost:8010 --name Client1
	# java --class-path "target/probench/jars/*" probench.cli client --node localhost:8010 --name Client2
	# java --class-path "target/probench/jars/*" probench.cli client --node localhost:8010 --name Client3

	trap 'kill $(jobs -p)' EXIT
	rm -r "target/probench/"

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

runProtoBenchEtcdCluster node="node":
	#!/usr/bin/env fish
	set -lx TOKEN token-01
	set -lx CLUSTER_STATE new
	set -lx NAME_1 machine-1
	set -lx NAME_2 machine-2
	set -lx NAME_3 machine-3
	set -lx HOST_1 localhost
	set -lx HOST_2 localhost
	set -lx HOST_3 localhost
	set -lx CLUSTER {$NAME_1}=http://{$HOST_1}:2381,{$NAME_2}=http://{$HOST_2}:2382,{$NAME_3}=http://{$HOST_3}:2383

	rm -rf /tmp/data*.etcd

	kitty etcd --data-dir=/tmp/data1.etcd --name {$NAME_1} --initial-advertise-peer-urls http://localhost:2381 --listen-peer-urls http://localhost:2381 --advertise-client-urls http://localhost:8010 --listen-client-urls http://localhost:8010 --initial-cluster {$CLUSTER} --initial-cluster-state {$CLUSTER_STATE} --initial-cluster-token token-01 &
	sleep 1;
	kitty etcd --data-dir=/tmp/data2.etcd --name {$NAME_2} --initial-advertise-peer-urls http://localhost:2382 --listen-peer-urls http://localhost:2382 --advertise-client-urls http://localhost:8020 --listen-client-urls http://localhost:8020 --initial-cluster {$CLUSTER} --initial-cluster-state {$CLUSTER_STATE} --initial-cluster-token token-01 &
	sleep 1;
	kitty etcd --data-dir=/tmp/data3.etcd --name {$NAME_3} --initial-advertise-peer-urls http://localhost:2383 --listen-peer-urls http://localhost:2383 --advertise-client-urls http://localhost:8030 --listen-client-urls http://localhost:8030 --initial-cluster {$CLUSTER} --initial-cluster-state {$CLUSTER_STATE} --initial-cluster-token token-01 &

	trap 'kill $(jobs -p)' SIGINT
	sleep $last_pid

runProtoBenchClientEtcd runId benchmark="put-100k-1C1N" benchResultsDir="bench-results/":
	#!/usr/bin/env fish

	set -l jarspath (sbt --error "print proBench/packageJars")

	cd "Modules/Examples/Protocol Benchmarks"

	set -lx BENCH_RESULTS_DIR {{benchResultsDir}}
	set -lx RUN_ID {{runId}}

	for clientArgs in $(cat ./benchmarks/{{benchmark}})
		set -l args (string split ';' $clientArgs)

		cat args/$args[3] | java --class-path "$jarspath/*" probench.cli etcd-client --name $args[1] --endpoints https://$args[2] &
	end

	trap 'kill $(jobs -p)' SIGINT
	sleep $last_pid


runProtoBenchClusterScript nodecount="15":
	scala-cli --jvm=system scripts/run-benchmark-nodes.scala -- --jars "Modules/Examples/Protocol Benchmarks/target/scala-3.5.2/jars" --nodes {{nodecount}}
