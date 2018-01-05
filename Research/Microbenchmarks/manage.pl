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

my $EXECUTABLE = './target/universal/stage/bin/microbenchmarks';
if ($OSNAME eq "MSWin32") {
  $EXECUTABLE =~ s#/#\\#g;
}
my $OUTDIR = 'out';
my $RESULTDIR = 'results';
my $SCHEDULER_TIME = "0:55:00";
my $SCHEDULER_REQUIRE = "avx\\&mpi";
my $SCHEDULER_CORES = "16";

# my @ENGINES = qw<parrp stm synchron locksweep>;
my @ENGINES = qw<synchron>;
# my @ENGINES_UNMANAGED = (@ENGINES, "unmanaged");
my @ENGINES_UNMANAGED = (@ENGINES);
my @ENGINES_SNAPSHOTS = ("synchron", "restoring");
my @THREADS = (1);
my @REDUCED_THREADS = (8);
my @STEPS = (1,8,16,24,32,64);
my @SIZES = (100);
my @CHATSERVERSIZES = (1,2,4,8,16,32);
my @PHILOSOPHERS = (16, 32, 64, 128);
my @SNAPSHOT_FOLDPERCENT = map {$_ / 10} (0..10);
my @LAYOUTS = qw<alternating>;
my %BASECONFIG = (
  # global locking does not deal well with sync iterations
  si => "false", # synchronize iterations
  wi => 5, # warmup iterations
  w => "1000ms", # warmup time
  f => 5, # forks
  i => 5, # iterations
  r => "1000ms", # time per iteration
  to => "10s", #timeout
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
# my @RUN = @ARGV ? @ARGV : qw<philosophers halfDynamicPhilosophers simplePhil expensiveConflict singleDynamic singleVar turnCreation simpleFan simpleReverseFan simpleNaturalGraph multiReverseFan stmbank chatServer simpleChain dynamicStacks noconflictPhilosophers>;
my @RUN = @ARGV ? @ARGV : qw<snapshotOverhead snapshotRestoringVsInitial snapshotRestoringVsRecomputation errorPropagationVsMonadic simpleNaturalGraph>;
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
  chdir "../..";

  system('./sbt', 'set scalacOptions in ThisBuild ++= List("-Xdisable-assertions", "-Xelide-below", "9999999")',
    'project microbench', 'jmh:compile', 'jmh:stage');
  chdir $MAINDIR;
}

sub run {
  my @runs = makeRuns();
  for my $run (@runs) {
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

    philosophers => sub {
      my @runs;

      for my $threads (@THREADS) {
        for my $layout (@LAYOUTS) {
          for my $phils (($layout eq "noconflict") ? $PHILOSOPHERS[-1] : @PHILOSOPHERS) {
            my $name = "philosophers-threads-$threads-layout-$layout-philosophers-$phils";
            my $program = makeRunString( $name,
              fromBaseConfig(
                p => { # parameters
                  tableType => 'static',
                  engineName => (join ',', @ENGINES_UNMANAGED),
                  philosophers => $phils,
                  layout => $layout,
                },
                t => $threads, #threads
              ),
              "philosophers"
            );
            push @runs, {name => $name, program => $program};
          }
        }
      }

      @runs;
    },

    noconflictPhilosophers => sub {
      my @runs;

      for my $threads (@THREADS) {
        for my $layout ("noconflict") {
          for my $phils ($PHILOSOPHERS[-1]) {
            my $name = "philosophers-threads-$threads-layout-$layout-philosophers-$phils";
            my $program = makeRunString( $name,
              fromBaseConfig(
                p => { # parameters
                  tableType => 'static,other',
                  engineName => (join ',', @ENGINES_UNMANAGED),
                  philosophers => $phils,
                  layout => $layout,
                },
                t => $threads, #threads
              ),
              "philosophers"
            );
            push @runs, {name => $name, program => $program};
          }
        }
      }

      @runs;
    },


    dynamicPhilosophers => sub {
      my @runs;

      for my $threads (@THREADS) {
        for my $layout (@LAYOUTS) {
          for my $phils (($layout eq "noconflict") ? $PHILOSOPHERS[-1] : @PHILOSOPHERS) {
            my $name = "dynamicPhilosophers-threads-$threads-layout-$layout-philosophers-$phils";
            my $program = makeRunString( $name,
              fromBaseConfig(
                p => { # parameters
                  tableType => 'dynamic',
                  engineName => (join ',', @ENGINES_UNMANAGED),
                  philosophers => $phils,
                  layout => $layout,
                  minBackoff => 100, maxBackoff => 1000000, factorBackoff => 1.1,
                },
                t => $threads, #threads
              ),
              "philosophers"
            );
            push @runs, {name => $name, program => $program};
          }
        }
      }

      @runs;
    },

    halfDynamicPhilosophers => sub {
      my @runs;

      for my $threads (@THREADS) {
        for my $layout (@LAYOUTS) {
          for my $phils (($layout eq "noconflict") ? $PHILOSOPHERS[-1] : @PHILOSOPHERS) {
            my $name = "halfDynamicPhilosophers-threads-$threads-layout-$layout-philosophers-$phils";
            my $program = makeRunString( $name,
              fromBaseConfig(
                p => { # parameters
                  tableType => 'other',
                  engineName => (join ',', @ENGINES_UNMANAGED),
                  philosophers => $phils,
                  layout => $layout,
                },
                t => $threads, #threads
              ),
              "philosophers"
            );
            push @runs, {name => $name, program => $program};
          }
        }
      }

      @runs;
    },

    backoff => sub {
      my @runs;

      for my $threads (1,2,4,8,12,16) {
        for my $layout ("alternating") {
          for my $tableType (qw<static dynamic>) {
            for my $backoff (
              [minBackoff => 0, maxBackoff => 0, factorBackoff => 1],
              [minBackoff => 10, maxBackoff => 10000000, factorBackoff => 1.3],
              [minBackoff => 10000, maxBackoff => 1000000, factorBackoff => 1.1],
              [minBackoff => 10000, maxBackoff => 10000000, factorBackoff => 1.1],
              [minBackoff => 100000, maxBackoff => 100000, factorBackoff => 1.0],
              ) {
              my $name = "backoff-threads-$threads-layout-$layout-type-$tableType-backoff-". join "-", @$backoff;
              my $program = makeRunString($name,
                fromBaseConfig(
                  p => { # parameters
                    tableType => $tableType,
                    engineName => "parrp",
                    philosophers => 48,
                    layout => $layout,
                    @$backoff
                  },
                  t => $threads, #threads
                  wi => 5, # warmup iterations
                  f => 1, # forks
                  i => 5, # iterations
                ),
                "philosophers"
              );
              push @runs, {name => $name, program => $program};
            }
          }
        }
      }

      @runs;
    },

    dynamicStacks => sub {
      my @runs;

      for my $threads (@REDUCED_THREADS) {
        for my $steps (@STEPS) {

          my $name = "dynamicStacks-threads-$threads-steps-$steps";
          my $program = makeRunString( $name,
            fromBaseConfig(
              p => { # parameters
                engineName => (join ',', @ENGINES_UNMANAGED),
                work => 0,
                size => 10,
                steps => $steps,
              },
              t => $threads, #threads
            ),
            "dynamic.Stacks"
          );
          push @runs, {name => $name, program => $program};
        }
      }
      @runs;
    },

    expensiveConflict => sub {
      my @runs;

      for my $work (0,100,500,1000,3000,5000,7500,10000) {
        my $name = "expensiveConflict-work-$work";
        my $program = makeRunString( $name,
          fromBaseConfig(
            p => { # parameters
              engineName => (join ',', @ENGINES),
              work => $work,
              minBackoff => 0, maxBackoff => 0, factorBackoff => 1.0,
            },
            t => 2, #threads
          ),
          "ExpensiveConflict"
        );
        push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    reference => sub {
      my @runs;

      for my $size (@THREADS) {
          my $name = "reference-threads-$size";
          my $program = makeRunString( $name,
            fromBaseConfig(
              p => { # parameters
                work => 2000,
              },
              t => $size, #threads
            ),
            "WorkReference"
          );
          push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    singleDynamic => sub {
      my @runs;

      for my $threads (@REDUCED_THREADS) {
        for my $steps (@STEPS) {
            my $name = "singleDynamic-steps-$steps-threads-$threads";
            my $program = makeRunString( $name,
              fromBaseConfig(
                p => { # parameters
                  engineName => (join ',', @ENGINES_UNMANAGED),
                  step =>  $steps
                },
                t => $threads,
              ),
              "dynamic.SingleSwitch"
            );
            push @runs, {name => $name, program => $program};
        }
      }

      @runs;
    },

    singleVar => sub {
      my @runs;

      for my $threads (@REDUCED_THREADS) {
          my $name = "singleVar-threads-$threads";
          my $program = makeRunString( $name,
            fromBaseConfig(
              p => { # parameters
                engineName => (join ',', @ENGINES_UNMANAGED),
              },
              t => $threads,
            ),
            "simple.SingleVar"
          );
          push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    turnCreation => sub {
      my @runs;

      for my $threads (@REDUCED_THREADS) {
          my $name = "turnCreation-threads-$threads";
          my $program = makeRunString( $name,
            fromBaseConfig(
              p => { # parameters
                engineName => (join ',', @ENGINES_UNMANAGED),
              },
              t => $threads,
            ),
            "simple.TurnCreation"
          );
          push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    creation => sub {
      my @runs;

      for my $threads (@THREADS) {
          my $name = "creation-threads-$threads";
          my $program = makeRunString( $name,
            fromBaseConfig(
              p => { # parameters
                engineName => (join ',', @ENGINES_UNMANAGED),
              },
              t => $threads,
            ),
            "simple.Creation"
          );
          push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    simplePhil => sub {
      my @runs;

      for my $threads (@THREADS) {
          my $name = "simplePhil-threads-$threads";
          my $program = makeRunString( $name,
            fromBaseConfig(
              p => { # parameters
                engineName => (join ',', @ENGINES, "unmanaged"),
              },
              t => $threads,
            ),
            "simple.SimplePhil"
          );
          push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    simpleChain => sub {
      my @runs;

      for my $threads (@REDUCED_THREADS) {
        for my $size (@SIZES) {
            my $name = "simpleChain-size-$size-threads-$threads";
            my $program = makeRunString( $name,
              fromBaseConfig(
                p => { # parameters
                  engineName => (join ',', @ENGINES_UNMANAGED),
                  size => $size,
                },
                t => $threads,
              ),
              "benchmarks.simple.Chain"
            );
            push @runs, {name => $name, program => $program};
        }
      }

      @runs;
    },

    simpleFan => sub {
      my @runs;

      for my $threads (@REDUCED_THREADS) {
        for my $size (@SIZES) {
            my $name = "simpleFan-size-$size-threads-$threads";
            my $program = makeRunString( $name,
              fromBaseConfig(
                p => { # parameters
                  engineName => (join ',', @ENGINES_UNMANAGED),
                  size => $size,
                },
                t => $threads,
              ),
              "benchmarks.simple.Fan"
            );
            push @runs, {name => $name, program => $program};
        }
      }
      @runs;
    },

    simpleReverseFan => sub {
      my @runs;

      for my $threads (@THREADS) {
          my $name = "simpleReverseFan-threads-$threads";
          my $program = makeRunString( $name,
            fromBaseConfig(
              p => { # parameters
                engineName => (join ',', @ENGINES_UNMANAGED),
              },
              t => $threads,
            ),
            "benchmarks.simple.ReverseFan"
          );
          push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    simpleNaturalGraph => sub {
      my @runs;

      for my $threads (@THREADS) {
        my $name = "simpleNaturalGraph-threads-$threads";
        my $program = makeRunString( $name,
          fromBaseConfig(
            p => { # parameters
              engineName => (join ',', @ENGINES_UNMANAGED),
            },
            t => $threads
          ),
          "benchmarks.simple.NaturalGraph"
        );
        push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    multiReverseFan => sub {
      my @runs;

      for my $threads (@REDUCED_THREADS) {
          my $name = "multiReverseFan-threads-$threads";
          my $program = makeRunString( $name,
            fromBaseConfig(
              p => { # parameters
                engineName => (join ',', @ENGINES_UNMANAGED),
                size => 4,
              },
              t => $threads,
            ),
            "benchmarks.simple.MultiReverseFan"
          );
          push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    stmbank => sub {
      my @runs;

      for my $size (16) {
        for my $run (0,1,2,5,7,10,15,20,25,30,35,40,50,60,70,80,90,100) {
          my $chance = 0.16 * $run;
          my $name = "stmbank-threads-$size-chance-$chance";
          my $program = makeRunString($name,
            fromBaseConfig(
              p => { # parameters
                engineName => (join ',', @ENGINES_UNMANAGED),
                numberOfAccounts => 256,
                readWindowCount => "4,8,16,32,64",
                globalReadChance => $chance,
              },
              t => $size, #threads
            ),
            "STMBank.BankAccounts"
          );
          push @runs, {name => $name, program => $program};
        }
      }

      @runs;
    },


    chatServer => sub {
      my @runs;

      for my $threads (@THREADS) {
          my $name = "chatServer-threads-$threads";
          my $program = makeRunString( $name,
            fromBaseConfig(
              p => { # parameters
                engineName => (join ',', @ENGINES_UNMANAGED),
                size => (join ",", @CHATSERVERSIZES),
                work => 0,
              },
              t => $threads,
            ),
            "benchmarks.chatserver.ChatBench"
          );
          push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    snapshotOverhead => sub {
      my @runs;

      my $name = "snapshotOverhead";
      my $program = makeRunString( $name,
        fromBaseConfig(
          p => { # parameters
            engineName => (join ',', @ENGINES_SNAPSHOTS),
            size => (join ",", @SIZES),
            foldPercent => (join ",", @SNAPSHOT_FOLDPERCENT)
          },
          t => 1,
        ),
        "benchmarks.restoring.RestoringSimple"
      );
      push @runs, {name => $name, program => $program};

      @runs;
    },

    snapshotRestoringVsInitial => sub {
      my @runs;

      my $name = "RestoringSnapshotVsInitial";
      my $program = makeRunString( $name,
        fromBaseConfig(
          p => { # parameters
            size => (join ",", @SIZES),
          },
          t => 1,
        ),
        "benchmarks.restoring.RestoringSnapshotVsInitial"
      );
      push @runs, {name => $name, program => $program};

      @runs;
    },

    snapshotRestoringVsRecomputation => sub {
      my @runs;

      my $name = "RestoringSnapshotVsRecomputation";
      my $program = makeRunString( $name,
        fromBaseConfig(
          p => { # parameters
            size => (join ",", (1,10,100,1000,10000)),
          },
          t => 1,
        ),
        "benchmarks.restoring.RestoringSnapshotVsRecomputation"
      );
      push @runs, {name => $name, program => $program};

      @runs;
    },

    errorPropagationVsMonadic => sub {
      my @runs;

      my $name = "errorPropagationVsMonadic";
      my $program = makeRunString( $name,
        fromBaseConfig(
          p => { # parameters
            engineName => (join ',', "synchron"),
            size => (join ",", @SIZES),
          },
          t => 1,
        ),
        "benchmarks.errorprop.MonadicErrors"
      );
      push @runs, {name => $name, program => $program};

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
#SBATCH -t 00:55:00
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

rm -r /tmp/\$(whoami)
mkdir /tmp/\$(whoami)
export LANG=en_US.UTF-8
export JAVA_OPTS="-Xmx1024m -Xms1024m -Djava.io.tmpdir=/tmp/\$(whoami)"
$programstring
rm -r /tmp/\$(whoami)

ENDPROGRAM
}
