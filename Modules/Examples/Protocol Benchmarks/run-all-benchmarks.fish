#!/usr/bin/env fish

set -lx WARMUP 10
set -lx MEASUREMENT 30
set -lx KILL_AFTER (math $MEASUREMENT / 2)

if not set -q ITERATIONS
	set -lx ITERATIONS 5
end

#fish ./docker-benchmarks/write-3x1/run.sh
fish ./fish-benchmarks/write-3x1/run.fish
# fish ./docker-benchmarks/write-3x1-kill-n1/run.sh
