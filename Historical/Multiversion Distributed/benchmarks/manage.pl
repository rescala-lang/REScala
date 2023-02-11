#!/usr/bin/perl
use 5.010;

use strict;
use warnings;
use utf8;
use English;
no if $] >= 5.018, warnings => "experimental::smartmatch";

use Cwd 'abs_path';
use File::Basename;
use Data::Dumper;

my $MAINDIR = dirname(__FILE__);
chdir $MAINDIR;

my $EXECUTABLE = './target/universal/stage/bin/rescala-distributed-benchmarks';
if ($OSNAME eq "MSWin32") {
  $EXECUTABLE =~ s#/#\\#g;
}
my $OUTDIR = 'out';
my $RESULTDIR = 'results';
my $SCHEDULER_TIME = "0:30:00";
my $SCHEDULER_REQUIRE = "avx\\&mpi";
my $SCHEDULER_CORES = "16";

my @THREADS = (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16);
#my @THREADS = (1,2,4,8,16);
my @DELAY = (24);

my %BASECONFIG = (
  # global locking does not deal well with sync iterations
  #si => "false", # synchronize iterations
  wi => 5, # warmup iterations
  w => "5000ms", # warmup time
  f => 5, # forks
  i => 7, # iterations
  r => "5000ms", # time per iteration
  to => "60s", #timeout
);

# stop java from formating numbers with `,` instead of `.`
$ENV{'LANG'} = 'en_US.UTF-8';
# my $OWNPATH = abs_path($0);
# $OWNPATH =~ s#/[^/]*$##;
# my $JMH_CLASSPATH = qq[-cp "$OWNPATH/Benchmarks/target/scala-2.11/jmh-classes"];
# say $JMH_CLASSPATH;
# $ENV{'JAVA_OPTS'} = $JMH_CLASSPATH;

my $GITREF = qx[git show -s --format=%H HEAD];
chomp $GITREF;

my $command = shift @ARGV;
my @RUN = @ARGV ? @ARGV : qw<mapgrid loopback conflictdistance>;
say "selected: " . (join " ", sort @RUN);
say "available: " . (join " ", sort keys %{&selection()});

given($command) {
  when ("show") { say Dumper([ makeRuns() ]) }
  when ("count") { say "will execute ", scalar(makeRuns()), " jobs"}
  when ("clean") { clean() }
  when ("init") { init() }
  when ("run") { run() }
  when ("submit") { submitAll() }
  when ("submitsingle") { submitAllAsOne() }
};

sub init {
  mkdir $RESULTDIR;
  mkdir "$RESULTDIR/$GITREF";
  mkdir $OUTDIR;
  chdir "../../..";

  system('./sbt', 'set scalacOptions in ThisBuild ++= List("-Xdisable-assertions", "-Xelide-below", "9999999")',
    'project distributedBenchmarks', 'jmh:compile', 'jmh:stage');
  chdir $MAINDIR;
}

sub run {
  my @runs = makeRuns();for my $run (@runs) {
    my $prog = $run->{program};
    say "executing $prog";
    system($prog);
  }
}

sub submitAll {
  for my $run (makeRuns()) {
    say "submitting ", $run->{name};
    submit(hhlrjob($run->{name}, $run->{program}));
  }
}

sub submitAllAsOne {
  my $allRuns = join "\n", (map { $_->{program} } makeRuns());
  submit(hhlrjob("combined run", $allRuns));
}

sub submit {
  my ($job) = @_;
  open (my $SCHEDULER, "|-", qq[sbatch --exclusive -n 1 -c $SCHEDULER_CORES -C $SCHEDULER_REQUIRE -t $SCHEDULER_TIME ]);
  print $SCHEDULER $job;
  close $SCHEDULER;
}

sub makeRunString {
  my ($name, $args, @benchmarks) = @_;
  my %arguments = %$args;
  my %params = %{delete $arguments{p}};
  my $paramstring =
    (join ' ',
      (map {"-$_ " . $arguments{$_}} keys %arguments),
      (map {"-p $_=" . $params{$_}} keys %params),
      (join " ", @benchmarks)
    );
  qq[$EXECUTABLE -rf csv -rff "$RESULTDIR/$GITREF/$name.csv" $paramstring];
}

sub makeRuns {
  my @runs;
  for my $run (@RUN) {
    push @runs, selectRun($run);
  }
  @runs;
}

sub selectRun {
  my ($run) = @_;

  my %selection = %{selection()};
  if (defined $selection{$run}) {
    return $selection{$run}->();
  }
  else {
    say "unknown: $run";
    return ();
  }

}

sub fromBaseConfig {
  my %extend = (%BASECONFIG, @_);
  return \%extend;
}

sub selection {
  return {
    mapgrid => sub {
      my @runs;

      for my $threads (@THREADS) {
        for my $perHostWidth (1) {
          for my $perHostDepth (1) {
            for my $width (1) {
              for my $depth (1..8) {
                for my $delay (@DELAY) {
                  my $name = "dist-grid-delay-$delay-phw-$perHostWidth-phd-$perHostDepth-w-$width-d-$depth-threads-$threads";
                  my $program = makeRunString( $name,
                    fromBaseConfig(
                      p => { # parameters
                        widthNodesPerHost => $perHostWidth,
                        depthNodesPerHost => $perHostDepth,
                        widthHosts => $width,
                        depthHosts => $depth,
                        msDelay => $delay,
                      },
                      t => $threads, #threads
                    ),
                    "DistributedSignalMapGrid"
                  );
                  push @runs, {name => $name, program => $program};
                }
              }
            }
          }
        }
      }

      @runs;
    },
    loopback => sub {
      my @runs;

      for my $threads (@THREADS) {
        for my $delay (@DELAY) {
          my $name = "loop-back-delay-$delay-threads-$threads";
          my $program = makeRunString( $name,
            fromBaseConfig(
              p => { # parameters
                msDelay => $delay,
              },
              t => $threads, #threads
            ),
            "RemoteGlitchLoop"
          );
          push @runs, {name => $name, program => $program};
        }
      }

      @runs;
    },
    conflictdistance => sub {
      my @runs;

      for my $totaldepth (1..8) {
        for my $width (2) {
          for my $mergeAt (1 .. $totaldepth) {
            for my $delay (@DELAY) {
              my $name = "conflict-distance-delay-$delay-totaldepth-$totaldepth-width-$width-mergeAt-$mergeAt-threads-2";
              my $program = makeRunString( $name,
                fromBaseConfig(
                  p => { # parameters
                    totalLength => $totaldepth,
					threads => $width,
                    mergeAt => $mergeAt,
                    msDelay => $delay,
                  },
                  t => 1, #threads
                ),
                "ConflictDistances"
              );
              push @runs, {name => $name, program => $program};
            }
          }
        }
      }

      @runs;
    },
    remerge => sub {
      my @runs;

      for my $threads (@THREADS) {
        for my $width (2) {
          for my $totaldepth (1..8) {
            for my $mergeAt (1 .. $totaldepth) {
              for my $delay (@DELAY) {
                my $name = "dist-remerge-delay-$delay-w-$width-totaldepth-$totaldepth-remerge-$mergeAt-threads-$threads";
                my $program = makeRunString( $name,
                  fromBaseConfig(
                    p => { # parameters
                      widthHosts => $width,
                      totalLength => $totaldepth,
                mergeAt => $mergeAt,
                      msDelay => $delay,
                    },
                    t => $threads, #threads
                  ),
                  "DistributedSignalMapGrid"
                );
                push @runs, {name => $name, program => $program};
              }
            }
          }
        }
      }

      @runs;
    },

  };
}


sub hhlrjob {
  my ($name, $programstring) = @_;
  return  <<ENDPROGRAM;
#!/bin/sh
# Job name
#SBATCH -J REScalaBenchmark
#
# File / path where STDOUT will be written, the %.j is the job id
#SBATCH -o job-%j.out
#
# Request the time you need for execution in [hour:]minute
#SBATCH -t 00:30:00
#
# Required resources
#SBATCH -C avx&mpi
#
# Request vitual memory you need for your job in MB
#SBATCH --mem-per-cpu=128
#
# Request the number of compute slots you want to use
#SBATCH -n 1
#SBATCH -c 16
# request exclusive access
#SBATCH --exclusive
module unload openmpi
module load java
echo "--------- processors ------------------------"
lscpu
echo "--------- java version ----------------------"
java -version
echo "---------------------------------------------"

rm -r /tmp/\$(whoami)/\$SLURM_JOB_ID
mkdir -p /tmp/\$(whoami)/\$SLURM_JOB_ID
export LANG=en_US.UTF-8
export JAVA_OPTS="-Xmx1024m -Xms1024m -Djava.io.tmpdir=/tmp/\$(whoami)/\$SLURM_JOB_ID"
$programstring
rm -r /tmp/\$(whoami)/\$SLURM_JOB_ID

ENDPROGRAM
}
