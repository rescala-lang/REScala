#!/usr/bin/perl
use 5.010;

use strict;
use warnings;
use utf8;
use English;
use Cwd 'abs_path';
no if $] >= 5.018, warnings => "experimental::smartmatch";

use Data::Dumper;

my $EXECUTABLE = './Benchmarks/target/start';
if ($OSNAME eq "MSWin32") {
  $EXECUTABLE =~ s#/#\\#g;
}
my $OUTDIR = 'out';
my $RESULTDIR = 'results';
my @FRAMEWORKS = ("ParRP", "REScalaSTM", "REScalaSync");
my @ENGINES = qw< synchron spinning stm >;
my @THREADS = (1..16,24,32,64);

# stop java from formating numbers with `,` instead of `.`
$ENV{'LANG'} = 'en_US.UTF-8';
# my $OWNPATH = abs_path($0);
# $OWNPATH =~ s#/[^/]*$##;
# my $JMH_CLASSPATH = qq[-cp "$OWNPATH/Benchmarks/target/scala-2.11/jmh-classes"];
# say $JMH_CLASSPATH;
# $ENV{'JAVA_OPTS'} = $JMH_CLASSPATH;

my $command = shift @ARGV;
my @RUN = @ARGV ? @ARGV : qw< prim simple philosophers dynamicStacks expensiveConflict >;

say "selected @RUN";

given($command) {
  when ("show") { say Dumper([ makeRuns() ]) }
  when ("init") { init() }
  when ("run") { run() }
  when ("submit") { submitAll() }
  when ("available") { say join " ", keys %{&selection()} }
};

sub init {
  mkdir $RESULTDIR;
  mkdir $OUTDIR;
  mkdir "$RESULTDIR/$_" for @RUN;
  chdir "Benchmarks";
  system('sbt','clean', 'stage', 'compileJmh');
  #system('sbt','clean', 'jmh:compile', 'jmh:stage');
  chdir "..";
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

sub submit {
  my ($job) = @_;
  open (my $BSUB, "|-", "bsub");
  print $BSUB $job;
  close $BSUB;
}

sub makeRunString {
  my ($prefix, $name, $args, @benchmarks) = @_;
  my %arguments = %$args;
  my %params = %{delete $arguments{p}};
  my $paramstring =
    (join ' ',
      (map {"-$_ " . $arguments{$_}} keys %arguments),
      (map {"-p $_=" . $params{$_}} keys %params),
      (join " ", @benchmarks)
    );
  "$EXECUTABLE -rf csv -rff \"results/$prefix/$name.csv\" $paramstring"
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


sub selection {
  return {
    simple => sub {
      my @runs;

      for my $size (@THREADS) {
        my $name = "threads-$size";
        my $program = makeRunString("simple", $name,
          {
            p => { # parameters
              riname => (join ',', @FRAMEWORKS),
            },
            si => "false", # synchronize iterations
            wi => 20, # warmup iterations
            w => "1000ms", # warmup time
            f => 5, # forks
            i => 10, # iterations
            r => "1000ms", # time per iteration
            t => $size, #threads
            to => "10s", #timeout
          },
          "simple.Mapping"
        );
        push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    prim => sub {
      my @runs;

      for my $size (@THREADS) {
        my $name = "size-$size";
        my $program = makeRunString("prim", $name,
          {
            p => { # parameters
              depth => 64,
              sources => 64,
              riname => (join ',', @FRAMEWORKS),
            },
            si => "false", # synchronize iterations
            wi => 20, # warmup iterations
            w => "1000ms", # warmup time
            f => 5, # forks
            i => 10, # iterations
            r => "1000ms", # time per iteration
            t => $size, #threads
            to => "10s", #timeout
          },
          ".*prim"
        );
        push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    philosophers => sub {
      my @runs;

      for my $size (@THREADS) {
        for my $layout (qw<alternating random third block>) {
          my $name = "threads-$size-layout-$layout";
          my $program = makeRunString("philosophers", $name,
            {
              p => { # parameters
                tableType => 'static',
                engineName => (join ',', @ENGINES),
                philosophers => "32,64,256",
                layout => $layout,
              },
              si => "false", # synchronize iterations
              wi => 20, # warmup iterations
              w => "1000ms", # warmup time
              f => 5, # forks
              i => 10, # iterations
              r => "1000ms", # time per iteration
              t => $size, #threads
              to => "10s", #timeout
            },
            "philosophers"
          );
          push @runs, {name => $name, program => $program};
        }
      }

      @runs;
    },

    dynamicStacks => sub {
      my @runs;

      for my $size (@THREADS) {
        my $name = "threads-$size";
        my $program = makeRunString("dynamicStacks", $name,
          {
            p => { # parameters
              engineName => (join ',', @ENGINES),
              work => 0,
            },
            si => "false", # synchronize iterations
            wi => 20, # warmup iterations
            w => "1000ms", # warmup time
            f => 5, # forks
            i => 10, # iterations
            r => "1000ms", # time per iteration
            t => $size, #threads
            to => "10s", #timeout
          },
          "dynamic.Stacks"
        );
        push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    expensiveConflict => sub {
      my @runs;

      for my $work (0,100,500,1000,3000,5000,7500,10000) {
        my $name = "work-$work";
        my $program = makeRunString("expensiveConflict", $name,
          {
            p => { # parameters
              engineName => (join ',', @ENGINES),
              work => $work,
            },
            si => "false", # synchronize iterations
            wi => 20, # warmup iterations
            w => "1000ms", # warmup time
            f => 5, # forks
            i => 10, # iterations
            r => "1000ms", # time per iteration
            t => 2, #threads
            to => "10s", #timeout
          },
          "ExpensiveConflict"
        );
        push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    reference => sub {
      my @runs;

      for my $size (@THREADS) {
          my $name = "threads-$size";
          my $program = makeRunString("reference", $name,
            {
              p => { # parameters
                work => 2000,
              },
              si => "false", # synchronize iterations
              wi => 5, # warmup iterations
              w => "1000ms", # warmup time
              f => 5, # forks
              i => 5, # iterations
              r => "1000ms", # time per iteration
              t => $size, #threads
              to => "10s", #timeout
            },
            "WorkReference"
          );
          push @runs, {name => $name, program => $program};
      }

      @runs;
    },

    stmbank => sub {
      my @runs;

      for my $size (@THREADS) {
        for my $chance ("0.01", "0.001", "0") {
          my $name = "threads-$size-$chance";
          my $program = makeRunString("stmbank", $name,
            {
              p => { # parameters
                riname => (join ',', @FRAMEWORKS),
                numberOfAccounts => 256,
                globalReadChance => $chance,
              },
              si => "false", # synchronize iterations
              wi => 20, # warmup iterations
              w => "1000ms", # warmup time
              f => 5, # forks
              i => 10, # iterations
              r => "1000ms", # time per iteration
              t => $size, #threads
              to => "10s", #timeout
            },
            "STMBank.BankAccounts"
          );
          push @runs, {name => $name, program => $program};
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
#BSUB -J REScalaBenchmark
#
# File / path where STDOUT will be written, the %J is the job id
#BSUB -o $OUTDIR/$name-%J.out
#
# File / path where STDERR will be written, the %J is the job id
# #BSUB -e $OUTDIR/$name-%J.err
#
# Request the time you need for execution in [hour:]minute
#BSUB -W 1:30
#
# Request vitual memory you need for your job in MB
#BSUB -M 2048
#
# Request the number of compute slots you want to use
#BSUB -n 16
#BSUB -q deflt
# request exclusive access
#BSUB -x

module unload openmpi
module load java
echo "--------- processors ---------"
nproc
echo "------------------------------"
who
echo "------------------------------"
ls -al /work/local
echo "------------------------------"

rm -r /tmp/\$(whoami)
mkdir /tmp/\$(whoami)
export JAVA_OPTS="-Xmx1024m -Xms1024m -Djava.io.tmpdir=/tmp/\$(whoami)"
$programstring
rm -r /tmp/\$(whoami)

ENDPROGRAM
}
