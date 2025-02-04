#!/usr/bin/env fish

set -lx BENCH_RESULTS_DIR benchmark-results
set -lx MODE write

for i in (seq $ITERATIONS);
	set -lx RUN_ID cluster3-put-{$WARMUP}_{$MEASUREMENT}_k3a{$KILL_AFTER}-run$i
	set -l killtime (math $WARMUP + $KILL_AFTER)
	set -l timeout (math (math (math $WARMUP + $MEASUREMENT) + 5) - $killtime)
	#set -l timeout (math (math $WARMUP + $MEASUREMENT) + 5)

	echo Iteration $i

	docker-compose -f ./docker-benchmarks/write-3x1-kill-n1/docker-compose.yml up -d
	sleep $killtime
	docker-compose -f ./docker-benchmarks/write-3x1-kill-n1/docker-compose.yml stop peer3
	sleep $timeout
	docker-compose -f ./docker-benchmarks/write-3x1-kill-n1/docker-compose.yml stop
	docker-compose -f ./docker-benchmarks/write-3x1-kill-n1/docker-compose.yml down
end
