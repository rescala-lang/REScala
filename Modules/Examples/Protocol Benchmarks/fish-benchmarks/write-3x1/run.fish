#!/usr/bin/env fish

set -lx BENCH_RESULTS_DIR benchmark-results
set -lx MODE write
set -lx ID (random)

if not set -q jarspath
	set -l oldPath $PWD
	cd ../../../
	set -g jarspath (sbt --error "print proBench/packageJars")
	cd $oldPath
end

set -lx SYSTEM_ID pb
for i in (seq $ITERATIONS);
	set -lx RUN_ID cluster3-put-$TIMES-id{$ID}_run$i
	set -l timeout (math (math $WARMUP + $MEASUREMENT) + 5)

	echo $jarspath
	echo Iteration $i

	# start cluster
	java --class-path "$jarspath/*" probench.cli node --name NODE1 --listen-client-port 8010 --listen-peer-port 8011 --cluster --initial-cluster-ids NODE1 NODE2 NODE3 > /dev/null &
	set -l NODE1 (jobs -pl)
	sleep 1

	java --class-path "$jarspath/*" probench.cli node --name NODE2 --listen-client-port 8020 --listen-peer-port 8021 --cluster localhost:8011 --initial-cluster-ids NODE1 NODE2 NODE3 > /dev/null &
	set -l NODE2 (jobs -pl)
	sleep 1

	java --class-path "$jarspath/*" probench.cli node --name NODE3 --listen-client-port 8030 --listen-peer-port 8031 --cluster localhost:8011 localhost:8021 --initial-cluster-ids NODE1 NODE2 NODE3 > /dev/null &
	set -l NODE3 (jobs -pl)
	sleep 1

	# start client
	for clientArgs in $(cat ./benchmarks/$BENCHMARK)
		set -l args (string split ';' $clientArgs)

		cat args/$args[3] | java --class-path "$jarspath/*" probench.cli client --node $args[2] --name $args[1] &
	end
    set -l CLIENT (jobs -pl)
	trap 'kill $(jobs -p)' SIGINT

	# stop everything after timeout
	#sleep $timeout
	wait $CLIENT
	jobs
	kill $NODE1 $NODE2 $NODE3
	sleep 5
	jobs
end

set -lx SYSTEM_ID etcd
for i in (seq $ITERATIONS);
	set -lx RUN_ID cluster3-put-$TIMES-id{$ID}_run$i
	echo Iteration $i

	# start etcd cluster
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

	etcd --data-dir=/tmp/data1.etcd --name {$NAME_1} --initial-advertise-peer-urls http://localhost:2381 --listen-peer-urls http://localhost:2381 --advertise-client-urls http://localhost:8010 --listen-client-urls http://localhost:8010 --initial-cluster {$CLUSTER} --initial-cluster-state {$CLUSTER_STATE} --initial-cluster-token token-01 > /dev/null &
	sleep 1;
	etcd --data-dir=/tmp/data2.etcd --name {$NAME_2} --initial-advertise-peer-urls http://localhost:2382 --listen-peer-urls http://localhost:2382 --advertise-client-urls http://localhost:8020 --listen-client-urls http://localhost:8020 --initial-cluster {$CLUSTER} --initial-cluster-state {$CLUSTER_STATE} --initial-cluster-token token-01 > /dev/null &
	sleep 1;
	etcd --data-dir=/tmp/data3.etcd --name {$NAME_3} --initial-advertise-peer-urls http://localhost:2383 --listen-peer-urls http://localhost:2383 --advertise-client-urls http://localhost:8030 --listen-client-urls http://localhost:8030 --initial-cluster {$CLUSTER} --initial-cluster-state {$CLUSTER_STATE} --initial-cluster-token token-01 > /dev/null &
	sleep 1

	# start client
    java --class-path "$jarspath/*" probench.cli benchmark-client --name client1 --node localhost:8010 --times $TIMES --mode $MODE &
    set -l CLIENT (jobs -pl)

	for clientArgs in $(cat ./benchmarks/$BENCHMARK)
		set -l args (string split ';' $clientArgs)

		cat args/$args[3] | java --class-path "$jarspath/*" probench.cli etcd-client --name $args[1] --endpoints https://$args[2] &
	end

    set -l CLIENT (jobs -pl)
	trap 'kill $(jobs -p)' SIGINT

	# wait for client
	wait $CLIENT
	jobs
	kill (jobs -p)
	sleep 5
	jobs
end
