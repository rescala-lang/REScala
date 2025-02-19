#!/usr/bin/env fish

set -lx WARMUP 10
set -lx MEASUREMENT 30
#set -lx KILL_AFTER (math $MEASUREMENT / 4)
set -lx KILL_AFTER 10
set -lx TIMES 1000000

if not set -q ITERATIONS
	set -gx ITERATIONS 5
end

set -lx BENCH_RESULTS_DIR benchmark-results
set -lx MODE write

if not set -q jarspath
	set -l oldPath $PWD
	cd ../../../
	set -gx jarspath (sbt --error "print proBench/packageJars")
	cd $oldPath
end

#fish ./docker-benchmarks/write-3x1/run.sh
set -lx BENCHMARK "put-1m-1C1N"
fish ./fish-benchmarks/write-3x1/run.fish
set -lx BENCHMARK "get-1m-1C1N"
fish ./fish-benchmarks/write-3x1/run.fish
set -lx BENCHMARK "mixed-1m-1C1N"
fish ./fish-benchmarks/write-3x1/run.fish
#fish ./fish-benchmarks/write-3x1/onlyetcd.fish
#fish ./fish-benchmarks/write-3x1-kill-n1/run.fish
# fish ./docker-benchmarks/write-3x1-kill-n1/run.sh
