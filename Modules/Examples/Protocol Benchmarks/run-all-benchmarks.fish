#!/usr/bin/env fish

set -lx WARMUP 10
set -lx MEASUREMENT 30
#set -lx KILL_AFTER (math $MEASUREMENT / 4)
set -lx KILL_AFTER 10

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
#fish ./fish-benchmarks/write-3x1/run.fish
fish ./fish-benchmarks/write-3x1-kill-n1/run.fish
# fish ./docker-benchmarks/write-3x1-kill-n1/run.sh
