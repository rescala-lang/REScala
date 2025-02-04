#!/usr/bin/env fish

set -lx BENCH_RESULTS_DIR benchmark-results
set -lx MODE write

for i in (seq $ITERATIONS);
	set -lx RUN_ID cluster3-put-{$WARMUP}_{$MEASUREMENT}-run$i
	set -l timeout (math (math $WARMUP + $MEASUREMENT) + 5)

	echo Iteration $i

	docker-compose -f ./docker-benchmarks/write-3x1/docker-compose.yml up -d
	sleep $timeout
	docker-compose -f ./docker-benchmarks/write-3x1/docker-compose.yml stop
	docker-compose -f ./docker-benchmarks/write-3x1/docker-compose.yml down
end
