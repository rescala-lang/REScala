#!/usr/bin/perl
use 5.020;

use strict;
use warnings;
use utf8;
use experimental 'signatures';
no if $] >= 5.018, warnings => "experimental::smartmatch";

use DBI;
use Text::CSV_XS qw( csv );
use Data::Dumper;
use Chart::Gnuplot;
use File::Find;
use File::Path qw(make_path remove_tree);
use Cwd 'abs_path';

# combining standard deviations is not trivial, but would be possible:
# http://www.burtonsys.com/climate/composite_standard_deviations.html

my $DBPATH = ':memory:';
my $TABLE = 'results';
my $CSVDIR = 'resultStore';
my $OUTDIR = 'fig';
my $BARGRAPH = abs_path("bargraph.pl");

our $FONT = "Times";
our $FONTSIZE = "30";

our $NAME_FINE = "Handcrafted";
our $NAME_COARSE = "G-Lock";
our $NAME_FULLMV = "FullMV";
our $NAME_PARRP = "ParRP";
our $NAME_STM = "STM-RP";
our $NAME_RESTORING = "Snapshots";
our $NAME_SIMPLE = "Simple";

our $YTIC_COUNT = 4;
our $YRANGE_ROUND = 100;
our $BARGRAPH_YTICS = 2.0;
our $BARGRAPH_YFORMAT = "%1.1f";
our $LEGEND_POS = "off";
our $YRANGE = "[0:]";
our $XRANGE = "[:]";
our $GNUPLOT_TERMINAL = "pdf size 5,2.5";
our %MARGINS = (
    lmargin => 5.8,
    rmargin => 1.5,
    tmargin => 0.3,
    bmargin => 1.5,
  );
our $VERTICAL_LINE = undef;
our $X_VARYING = "Threads";
our $BARGRAPH_LEGEND = "legendx=inside";
our %ADDITIONAL_GNUPLOT_PARAMS = ();

our $MANUAL_BARGRAPH_HACK = 0;

sub prettyName($name) {
  $name =~ s/Param: engineName:\s*//;
  $name =~ s/pessimistic|spinning|REScalaSpin|parrp/$NAME_PARRP/;
  $name =~ s/fullmv/$NAME_FULLMV/;
  $name =~ s/stm|REScalaSTM|STM/$NAME_STM/;
  $name =~ s/synchron|REScalaSynchron/$NAME_COARSE/;
  $name =~ s/unmanaged/$NAME_FINE/;
  $name =~ s/restoring/$NAME_RESTORING/;
  $name =~ s/simple/$NAME_SIMPLE/;
  return $name;
}

sub styleByName($name) {
  given($name) {
    when (/$NAME_PARRP/)             { 'linecolor "dark-green"  lt 2 lw 2 pt 7  ps 1' }
    when (/$NAME_STM/)               { 'linecolor "blue"        lt 2 lw 2 pt 5  ps 1' }
    when (/$NAME_COARSE|Restore/)    { 'linecolor "red"         lt 2 lw 2 pt 9  ps 1' }
    when (/$NAME_SIMPLE/)            { 'linecolor "light-red"   lt 2 lw 2 pt 8  ps 1' }
    when (/fair/)                    { 'linecolor "light-blue"  lt 2 lw 2 pt 8  ps 1' }
    when (/$NAME_FULLMV/)            { 'linecolor "light-green" lt 2 lw 2 pt 5  ps 1' }
    when (/$NAME_FINE/)              { 'linecolor "black"       lt 2 lw 2 pt 11 ps 1' }
    when (/$NAME_RESTORING|Derive/)  { 'linecolor "dark-green"  lt 2 lw 2 pt 6  ps 1' }
    default { '' }
  }
}

my $DBH = DBI->connect("dbi:SQLite:dbname=". $DBPATH,"","",{AutoCommit => 0,PrintError => 1});
{

  importCSV();
#  $DBH->do("DELETE FROM $TABLE WHERE Threads > 16");
#  $DBH->do(qq[DELETE FROM $TABLE WHERE "Param: engineName" = "fair"]);
#  $DBH->do(qq[DELETE FROM $TABLE WHERE "Param: engineName" = "parrp"]);

  remove_tree($_) for glob("$OUTDIR/*");
  mkdir $OUTDIR;
  chdir $OUTDIR;

  makeLegend();
  # miscBenchmarks();
  # restorationBenchmarks();
  # mvrpBenchmarks();
  universeBenchmark();

  $DBH->commit();
}

sub restorationBenchmarks() {
  { # fold percentages
    local $LEGEND_POS = "left bottom";
    my $benchmark = "benchmarks.restoring.RestoringSimple.countMany";
    local $X_VARYING = "Param: foldPercent";
    for my $foldNodes (queryChoices("Param: size", Benchmark => $benchmark)) {
      #local $LEGEND_POS = "right top" if $foldNodes == 4;
      plotBenchmarksFor("folds", "foldPercentage$foldNodes",
        (map {{Title => $_, "Param: engineName" => $_ , Benchmark => $benchmark, "Param: size" => $foldNodes }}
          queryChoices("Param: engineName", Benchmark => $benchmark, "Param: size" => $foldNodes)),);
    }
  }

  { # resotring vs deriving
    local $X_VARYING = "Param: size";
    local $LEGEND_POS = "right top";
    local %ADDITIONAL_GNUPLOT_PARAMS = (logscale => "x 10");
    plotBenchmarksFor("restoreVsDerive", "restoreVsDerive",
      {Title => "Restore", Benchmark => "benchmarks.restoring.RestoringSnapshotVsRecomputationA.restored"},
      {Title => "Derive", Benchmark => "benchmarks.restoring.RestoringSnapshotVsRecomputationB.derived"});
  }
}

sub mvrpBenchmarks() {
  # paper philosophers
  for my $topper (queryChoices("Param: topper")) {
    for my $dynamic (queryChoices("Param: dynamicity", "Param: topper" => $topper)) {
      for my $philosophers (queryChoices("Param: philosophers", "Param: dynamicity" => $dynamic, "Param: topper" => $topper)) {
        # local $YRANGE = "[0:500]" if $philosophers <= 64 && $dynamic eq "static";
        # local $YRANGE = "[0:400]" if $philosophers <= 32 && $dynamic eq "static";
        # local $YRANGE = "[0:800]" if $philosophers > 64 && $dynamic eq "static";
        # local $YRANGE = "[0:300]" if $dynamic ne "static" && $philosophers <= 64;
        local $YRANGE = "[0:150]" if $philosophers == 16 && $topper eq "none";
        local $YRANGE_ROUND = 150 if $philosophers == 16 && $topper eq "none";
        local $YTIC_COUNT = 3 if $philosophers == 16 && $topper eq "none";
        local $YRANGE = "[0:400]" if $philosophers == 64 && $topper eq "none";
        local $YRANGE_ROUND = 400 if $philosophers == 64 && $topper eq "none";
        local $YRANGE = "[0:900]" if $philosophers == -4 && $topper eq "none";
        local $YRANGE_ROUND = 900 if $philosophers == -4 && $topper eq "none";
        plotBenchmarksFor("paperphilosophers-$topper", "$dynamic-$philosophers",
          map { {Title => $_, "Param: engineName" => $_ , Benchmark => "benchmarks.philosophers.PaperPhilosopherCompetition.eatOnce",
          "Param: philosophers" => $philosophers, "Param: dynamicity" => $dynamic, "Param: topper" => $topper } }
            queryChoices("Param: engineName", "Param: topper" => $topper, "Param: dynamicity" => $dynamic, "Param: philosophers" => $philosophers));
      }
    }
  }

  # topologies
  for my $work (queryChoices("Param: work", Benchmark => "benchmarks.simple.ReverseFan.run")) {
    plotBenchmarksFor("topologies", "reverseFan-work-$work",
        (map {{Title => $_, "Param: engineName" => $_, "Param: work" => $work, Benchmark => "benchmarks.simple.ReverseFan.run" }}
          queryChoices("Param: engineName", "Param: work" => $work, Benchmark => "benchmarks.simple.ReverseFan.run")),);
  }
  for my $work (queryChoices("Param: work", Benchmark => "benchmarks.simple.SignalMapGrid.run")) {
    for my $width (queryChoices("Param: width", "Param: work" => $work, Benchmark => "benchmarks.simple.SignalMapGrid.run")) {
      for my $depth (queryChoices("Param: depth", "Param: width" => $width, "Param: work" => $work, Benchmark => "benchmarks.simple.SignalMapGrid.run")) {
        my $name = "grid-w-$width-d-$depth";
        if($width == 0) {
          if($depth == 0) {
            $name = "singleSource";
          } else {
            $name = "singleSource-$depth";
          }
        } elsif ($width == 1) {
          $name = "chain-$depth";
        } elsif ($depth == 1) {
          $name = "fan-$width";
        }
        plotBenchmarksFor("topologies", "$name-work-$work",
            (map {{Title => $_, "Param: engineName" => $_, "Param: depth" => $depth, "Param: width" => $width, "Param: work" => $work, Benchmark => "benchmarks.simple.SignalMapGrid.run" }}
              queryChoices("Param: engineName", "Param: depth" => $depth, "Param: width" => $width, "Param: work" => $work, Benchmark => "benchmarks.simple.SignalMapGrid.run")),);
      }
    }
  }


}

sub universeBenchmark() {
  # universe
  #local $YRANGE = "[5:24] reverse";
  local $YRANGE_ROUND = 10;
  $DBH->do(qq[UPDATE $TABLE SET Score = 60 / Score WHERE Benchmark = "UniverseCaseStudy"]);
  plotBenchmarksFor("Universe", "Universe",
    (map {{Title => $_, "Param: engineName" => $_ , Benchmark => "UniverseCaseStudy" }}
        queryChoices("Param: engineName", Benchmark => "UniverseCaseStudy")));
}

sub miscBenchmarks() {
  for my $dynamic (queryChoices("Param: tableType")) {
    for my $philosophers (queryChoices("Param: philosophers", "Param: tableType" => $dynamic)) {
      #local $LEGEND_POS = "left top" if $philosophers == 48  || $philosophers == 16;
      for my $layout (queryChoices("Param: layout", "Param: tableType" => $dynamic, "Param: philosophers" => $philosophers)) {
        # local $YRANGE = "[0:500]" if $philosophers <= 64 && $dynamic eq "static";
        # local $YRANGE = "[0:800]" if $philosophers > 64 && $dynamic eq "static";
        # local $YRANGE = "[0:300]" if $dynamic ne "static" && $philosophers <= 64;
        # local $YRANGE = "[0:200]" if $dynamic ne "static" && $philosophers <= 32;
        # local $YRANGE = "[0:]" if $layout eq "third";
        # local $LEGEND_POS = "left top" if $layout eq "third";
        local $VERTICAL_LINE = 8 if $layout eq "noconflict";
        local $NAME_FINE = "No Synchron" if $layout eq "noconflict";
        #local $VERTICAL_LINE = $philosophers / 3 if $layout ne "third";
        plotBenchmarksFor("${dynamic}-philosophers-$philosophers", $layout,
          map { {Title => $_, "Param: engineName" => $_ , Benchmark => "benchmarks.philosophers.PhilosopherCompetition.eat",
          "Param: philosophers" => $philosophers, "Param: layout" => $layout, "Param: tableType" => $dynamic } }
            queryChoices("Param: engineName", "Param: tableType" => $dynamic, "Param: philosophers" => $philosophers, "Param: layout" => $layout));
        # for my $boFactor (queryChoices("Param: factorBackoff", "Param: layout" => $layout, "Param: tableType" => $dynamic, "Param: philosophers" => $philosophers)) {
        #   for my $boMax (queryChoices("Param: maxBackoff", "Param: factorBackoff" => $boFactor, "Param: layout" => $layout, "Param: tableType" => $dynamic, "Param: philosophers" => $philosophers)) {

        #     plotBenchmarksFor("${dynamic}philosophers$philosophers", "$layout-$boFactor-$boMax",
        #       map { {Title => $_, "Param: engineName" => $_ , Benchmark => "benchmarks.philosophers.PhilosopherCompetition.eat",
        #       "Param: philosophers" => $philosophers, "Param: layout" => $layout, "Param: tableType" => $dynamic, "Param: factorBackoff" => $boFactor } }
        #         queryChoices("Param: engineName", "Param: maxBackoff" => $boMax, "Param: factorBackoff" => $boFactor, "Param: tableType" => $dynamic, "Param: philosophers" => $philosophers, "Param: layout" => $layout));
        #   }
        # }
      }
    }

    my $byPhilosopher = sub($engine) {
      my @choices = sort {$a <=> $b } queryChoices("Param: philosophers", "Param: engineName" => $engine, "Param: layout" => "alternating", "Param: tableType" => $dynamic );
      map { {Title => $engine . " " . $_, "Param: engineName" => $engine , Benchmark => "benchmarks.philosophers.PhilosopherCompetition.eat",
        "Param: philosophers" => $_, "Param: layout" => "alternating", "Param: tableType" => $dynamic } } (
         @choices);
    };
    plotBenchmarksFor("${dynamic}-philosophers", "philosopher comparison engine scaling",
      map { $byPhilosopher->($_) } (queryChoices("Param: engineName", "Param: tableType" => $dynamic)));


    plotBenchmarksFor("${dynamic}-philosophers", "Philosopher Table",
      map { {Title => $_, "Param: engineName" => $_ , Benchmark =>  "benchmarks.philosophers.PhilosopherCompetition.eat", "Param: tableType" => $dynamic } }  queryChoices("Param: engineName", "Param: tableType" => $dynamic));


    { # varying conflict potential
      my $threads = 8;
      for my $layout (queryChoices("Param: layout", "Param: tableType" => $dynamic)) {
        my $query = queryDataset(query("Param: philosophers", "Benchmark", "Param: engineName", "Param: tableType", "Threads", "Param: layout"));
        plotDatasets("${dynamic}-philosophers", "${layout}-concurrency-scaling", {xlabel => "Philosophers"},
          map { $query->(prettyName($_), "benchmarks.philosophers.PhilosopherCompetition.eat", $_, $dynamic, $threads, $layout) } queryChoices("Param: engineName", "Param: tableType" => $dynamic, "Threads" => $threads, "Param: layout" => $layout));
      }
    }

  }

  plotChoices("backoff", "dynamic", "Param: minBackoff", "Param: engineName" => "parrp" , Benchmark => "benchmarks.philosophers.PhilosopherCompetition.eat",
          "Param: philosophers" => 48, "Param: layout" => "alternating", "Param: tableType" => "dynamic" );

  {
    for my $threads (8) {
      local $BARGRAPH_LEGEND = "=noxlabels
=nolegend";
      compareBargraph($threads, "bargraph", "parallelizable",
        [qw<synchron parrp fullmv stm unmanaged>],
        Structures => q[results.Benchmark = "benchmarks.basic.TurnCreation.run"],
        Read => q[results.Benchmark = "benchmarks.basic.SingleVar.read"],
        MultiFan => q[results.Benchmark = "benchmarks.simple.MultiReverseFan.run"],
        Build => q[results.Benchmark like "benchmarks.simple.SimplePhil.build"],
        Philosopher => q[(results.Benchmark = "benchmarks.philosophers.PhilosopherCompetition.eat"
                   AND `Param: tableType` = "static" AND `Param: layout` = "alternating")],
      );
      {
        local $BARGRAPH_YTICS = 0.2;
        #local $BARGRAPH_YFORMAT = "%1.2f";
        #local $MANUAL_BARGRAPH_HACK = 1;
        compareBargraph($threads, "bargraph", "non-parallelizable",
          [qw<synchron parrp fullmv stm unmanaged>],
          SingleSwitch => q[results.Benchmark = "benchmarks.dynamic.SingleSwitch.run"],
          SingleWrite => q[results.Benchmark = "benchmarks.basic.SingleVar.write"],
          ReverseFan => q[results.Benchmark = "benchmarks.simple.ReverseFan.run"],
          DynamicStack => q[results.Benchmark = "benchmarks.dynamic.Stacks.run"],
        );
      }
      compareBargraph($threads, "bargraph", "multiplied",
        [qw<synchron parrp fullmv stm unmanaged>],
        Natural => q[results.Benchmark = "benchmarks.simple.NaturalGraph.run"],
        SignalSeq => q[results.Benchmark = "benchmarks.simple.ChainSignal.run"],
        EventSeq => q[results.Benchmark = "benchmarks.simple.ChainEvent.run"],
        Fan => q[results.Benchmark = "benchmarks.simple.Fan.run"],
        Philosopher => q[results.Benchmark = "benchmarks.philosophers.PhilosopherCompetition.eat" AND "Param: layout" = "noconflict"]
      );
    }
  }


  { # natural per-thread copies
    plotBenchmarksFor("simple", "Natural",
      map {{Title => $_, "Param: work" => 0, "Param: engineName" => $_ , Benchmark => "benchmarks.simple.NaturalGraph.run" }}
        queryChoices("Param: engineName", Benchmark => "benchmarks.simple.NaturalGraph.run"));
  }

  { # simplePhil
    local $NAME_FINE = "No Synchron";
    for my $bench (qw< propagate build buildAndPropagate >) {
      #local $LEGEND_POS = "left top" if $bench eq "build";
      plotBenchmarksFor("simplePhil", "SimplePhil$bench",
        map {{Title => $_, "Param: engineName" => $_ , Benchmark => "benchmarks.simple.SimplePhil.$bench" }}
          queryChoices("Param: engineName", Benchmark => "benchmarks.simple.SimplePhil.$bench"));
    }
  }


  { # expensive conflict
    #local $LEGEND_POS = "center right";
    my $compareTo = "locksweep";
    $DBH->do(qq[UPDATE $TABLE SET "Param: work" = "Param: work" / 1000 WHERE Benchmark like ?],undef, "benchmarks.conflict.ExpensiveConflict%");
    my $query = queryDataset(query("Param: work", "Benchmark", "Param: engineName"));
    plotDatasets("conflicts", "Asymmetric Workloads", {xlabel => "Work"},
      $query->("cheap " . prettyName($compareTo), "benchmarks.conflict.ExpensiveConflict.g:cheap", "$compareTo"),
      $query->("expensive  " . prettyName($compareTo), "benchmarks.conflict.ExpensiveConflict.g:expensive", "$compareTo"),
      $query->("cheap " . prettyName("stm"), "benchmarks.conflict.ExpensiveConflict.g:cheap", "stm"),
      $query->("expensive " . prettyName("stm"), "benchmarks.conflict.ExpensiveConflict.g:expensive", "stm"));

    plotDatasets("conflicts", "STM aborts", {xlabel => "Work"},
      $query->("cheap ". prettyName("stm"), "benchmarks.conflict.ExpensiveConflict.g:cheap", "stm"),
      $query->("expensive ". prettyName("stm"), "benchmarks.conflict.ExpensiveConflict.g:expensive", "stm"),
      $query->("expensive tried ". prettyName("stm"), "benchmarks.conflict.ExpensiveConflict.g:tried", "stm"));
  }

  { # other
    for my $benchmark (grep {/Creation|SingleVar/} queryChoices("Benchmark")) {
      plotBenchmarksFor("other", $benchmark,
        map {{Title => $_, "Param: engineName" => $_ , Benchmark => $benchmark }}
          queryChoices("Param: engineName", Benchmark => $benchmark));
    }
  }


  { # chain, fan
    for my $benchmark (grep {/simple\.(Chain|Fan)/} queryChoices("Benchmark")) {
      for my $threads (queryChoices("Threads", Benchmark => $benchmark)) {
        my $query = queryDataset(query("Param: size", "Benchmark", "Param: engineName", "Threads"));
        plotDatasets("simple", $threads ."-". $benchmark, {xlabel => "Size", logscale => "x 10",},
          map { $query->(prettyName($_), $benchmark, $_, $threads) } queryChoices("Param: engineName", "Benchmark" => $benchmark, Threads => $threads));
      }
    }
  }


  { # switch
    for my $benchmark (grep {/dynamic\.SingleSwitch/} queryChoices("Benchmark")) {
      for my $threads (queryChoices("Threads", Benchmark => $benchmark)) {
        my $query = queryDataset(query("Param: step", "Benchmark", "Param: engineName", "Threads"));
        plotDatasets("simple", $threads ."-". $benchmark, {xlabel => "Step"},
          map { $query->(prettyName($_), $benchmark, $_, $threads) } queryChoices("Param: engineName", "Benchmark" => $benchmark, Threads => $threads));
      }
    }
  }

  # { # stmbank
  #   for my $globalReadChance (queryChoices("Param: globalReadChance")) {
  #     my $benchmark = "benchmarks.STMBank.BankAccounts.reactive";
  #     plotBenchmarksFor("BankAccounts", $globalReadChance,
  #       (map {{Title => $_, "Param: globalReadChance" => $globalReadChance, "Param: engineName" => $_ , Benchmark => $benchmark }}
  #         queryChoices("Param: engineName", Benchmark => $benchmark, "Param: globalReadChance" => $globalReadChance)),
  #       {Title => "Pure STM", "Param: globalReadChance" => $globalReadChance, Benchmark => "benchmarks.STMBank.BankAccounts.stm"});
  #   }
  # }

  { # stmbank 2
    my $benchmark = "benchmarks.STMBank.BankAccounts.reactive";
    local $YTIC_COUNT = 5;
    local $YRANGE_ROUND = 500;
    $DBH->do(qq[UPDATE $TABLE SET "Param: globalReadChance" = "Param: globalReadChance" / Threads WHERE Benchmark = ?],undef, $benchmark);
    for my $windows (queryChoices("Param: readWindowCount", Benchmark => $benchmark)) {
      #local $LEGEND_POS = "top right" if $windows == 8;
      local $X_VARYING = "Param: globalReadChance";
      #local $YRANGE = "[0:800]";
      plotChoices("BankAccounts", "readProbability$windows", "Param: engineName",
        "Param: readWindowCount" => $windows,
        Threads => 16,
        Benchmark => $benchmark);
      local $YRANGE = "[600:]";
      local $XRANGE = "[0:0.01]";
      plotChoices("BankAccounts", "readProbability${windows}Upper", "Param: engineName",
        "Param: readWindowCount" => $windows,
        Threads => 16,
        Benchmark => $benchmark);
    }
  }

  { # reverse fan
      my $benchmark = "benchmarks.simple.ReverseFan.run";
      #local $LEGEND_POS = "top right";
      local $VERTICAL_LINE = 8;
      plotBenchmarksFor("simple", "ReverseFan",
        (map {{Title => $_, "Param: engineName" => $_ , Benchmark => $benchmark }}
          queryChoices("Param: engineName", Benchmark => $benchmark)),);
  }

  { # multi reverse fan
      my $benchmark = "benchmarks.simple.MultiReverseFan.run";
      plotBenchmarksFor("simple", "MultiReverseFan",
        (map {{Title => $_, "Param: engineName" => $_ , Benchmark => $benchmark }}
          queryChoices("Param: engineName", Benchmark => $benchmark)),);
  }

  { # chatServer
    my $benchmark = "benchmarks.chatserver.ChatBench.chat";
    for my $rooms (queryChoices("Param: size", Benchmark => $benchmark)) {
      #local $LEGEND_POS = "right top" if $rooms == 4;
      local $VERTICAL_LINE = $rooms / 2;
      plotBenchmarksFor("ChatServer", "$rooms",
        (map {{Title => $_, "Param: engineName" => $_ , Benchmark => $benchmark, "Param: size" => $rooms }}
          queryChoices("Param: engineName", Benchmark => $benchmark, "Param: size" => $rooms)),);
    }
  }
}

sub query($varying, @keys) {
  my $where = join " AND ", map {qq["$_" = ?]} @keys;
  return qq[SELECT "$varying", sum(Score * Samples) / sum(Samples), min(Score), max(Score) FROM "$TABLE" WHERE $where GROUP BY "$varying" ORDER BY "$varying"];
}

sub queryChoices($key, %constraints) {
  my $where = join " AND ", (map {qq["$_" = ?]} keys %constraints), qq["$key" IS NOT NULL];
  return @{$DBH->selectcol_arrayref(qq[SELECT DISTINCT "$key" FROM "$TABLE" WHERE $where ORDER BY "$key"], undef, values %constraints)};
}

sub plotChoices($group, $name, $vary, @constraints) {
    plotBenchmarksFor($group, $name,
          map { {Title => "$vary: $_", $vary => $_, @constraints } }
            queryChoices($vary, @constraints));
}

sub plotBenchmarksFor($group, $name, @graphs) {
  my @datasets;
  for my $graph (@graphs) {
    my $title = delete $graph->{"Title"};
    my @keys = keys %{$graph};
    push @datasets, queryDataset(query($X_VARYING, @keys))->(prettyName($title) // "unnamed", values %{$graph});
  }
  plotDatasets($group, $name, {}, @datasets);
}

sub queryDataset($query) {
  my $sth = $DBH->prepare($query);
  return sub($title, @params) {
    $sth->execute(@params);
    my $data = $sth->fetchall_arrayref();
    return makeDataset($title, $data) if (@$data);
    say "query for ". Dumper($title) . " had no results: [$query] ". Dumper(@params);
    return;
  }
}

sub styling($name) {
  my $res = styleByName($name);
  given($name) {
    when (/(\d+)/) {
      my $pt = $1;
      $res =~ s/pt \d+/pt $pt/;
    }
    when (/cheap/) { $res =~ s/pt \d+/pt 9/; continue;}
    when (/expensive/) { $res =~ s/pt \d+/pt 7/; continue;}
    when (/tried/) {
      $res =~ s/pt \d+/pt 5/;
      $res =~ s/color "\w+"/color "red"/;
    }
  }
  $res;
}

sub makeDataset($title, $data) {
  $data = [sort {$a->[0] <=> $b->[0]} @$data];

  Chart::Gnuplot::DataSet->new(
    xdata => [map {$_->[0]} @$data],
    ydata => [map {$_->[2]} @$data],
    title => $title . "min",
    style => 'linespoints ' . styling($title),
  ),
  Chart::Gnuplot::DataSet->new(
    xdata => [map {$_->[0]} @$data],
    ydata => [map {$_->[3]} @$data],
    title => $title . "max",
    style => 'linespoints ' . styling($title),
  );
  Chart::Gnuplot::DataSet->new(
    xdata => [map {$_->[0]} @$data],
    ydata => [map {$_->[1]} @$data],
    title => $title,
    style => 'linespoints ' . styling($title),
  );
}

sub unmangleName($name) {
  return $name =~ s/\$u(\d{4})/chr(hex($1))/egr; # / highlighter
}

sub makeLegend() {
  my @datasets = map {
    my $name = prettyName(unmangleName($_));
    Chart::Gnuplot::DataSet->new(
      xdata => [10,20],
      ydata => [10,20],
      title => $name,
      style => 'linespoints ' . styling($name),
    )
    } queryChoices("Param: engineName");
  my $chart = Chart::Gnuplot->new(
    output => "legend.pdf",
    terminal => "$GNUPLOT_TERMINAL enhanced font '$FONT,$FONTSIZE'",
    key => "top left vertical",
    %MARGINS,
    xrange => "[0:1]",
    yrange => "[0:1]",
    noborder => "",
    noxtics => "",
    noytics => "",
    notitle => "",
    noxlabel => "",
    noylabel => "",
  );
  $chart->plot2d(@datasets);
}

sub roundTo($target, $value) {
  int($value / $target +1)* $target;
}

sub plotDatasets($group, $name, $additionalParams, @datasets) {
  mkdir $group;
  unless (@datasets) {
    say "dataset for $group/$name is empty";
    return;
  }
  #say Dumper(@datasets);
  my ($ymax,) = sort {$b <=> $a} map {@{$_->{ydata}}} @datasets;
  $ymax = roundTo($YRANGE_ROUND, $ymax);
  $name = unmangleName($name);
  my $nospecial = $name =~ s/\W/_/gr; # / highlighter
  my $chart = Chart::Gnuplot->new(
    output => "$group/$nospecial.pdf",
    terminal => "$GNUPLOT_TERMINAL enhanced font '$FONT,$FONTSIZE'",
    key => $LEGEND_POS,
    #title  => $name,
    #xlabel => "Active threads",
    xrange => $XRANGE,
    yrange => $YRANGE,
    ytics => $ymax/$YTIC_COUNT,
    #logscale => "x 2; set logscale y 10",
    #ylabel => "Operations per millisecond",
    # xrange => "reverse",
    #ylabel => "Ops/ms",
    %MARGINS,
    %$additionalParams,
    %ADDITIONAL_GNUPLOT_PARAMS
  );
  if (defined $VERTICAL_LINE) {
    $chart->line(
      from => "$VERTICAL_LINE,graph(0,0)",
      to => "$VERTICAL_LINE,graph(1,1)",
      color => "black",
    );
  }
  $chart->plot2d(@datasets);
}


sub compareBargraph($threads, $group, $name, $engines, %conditions) {

  my @engines = @$engines;
  my ($head, @tail) = @engines;
  my @pretty = map {prettyName($_)} @engines;
  mkdir $group;
  chdir $group;
  my $TMPFILE = "$threads$name.perf";
  open my $OUT, ">", $TMPFILE;
  say $OUT "=cluster " . (join ", ", @pretty) . "
=sortbmarks
yformat=$BARGRAPH_YFORMAT
xlabel=
# ylabel=Speedup vs. $pretty[0]
ylabelshift=2,0
font=$FONT
yscale=0.6666667
xscale=1.4
fontsz=19
colors=red,green,magenta,blue,black
=norotate
extraops=set ytics $BARGRAPH_YTICS
$BARGRAPH_LEGEND
=table,";

  if ($MANUAL_BARGRAPH_HACK) {
    pop @tail;
    pop @engines;
  }

  for my $name (keys %conditions) {
    my $bmcond = $conditions{$name};
    my $joinMapTail = sub ($f, $j = " AND ") { join $j, map { $f->() } @tail };
    my $paramEqualIfDefined = sub ($param) { qq[(`$head`.`$param` IS NULL OR (] . $joinMapTail->(sub { qq[$_.`$param` = `$head`.`$param` ] }) . qq[)) ] };
    sub sum($engine) { qq[ (sum($engine.Score * $engine.Samples) / sum($engine.Samples)) ] };
    my $queryString = qq[SELECT ] .
      (join ", ", map { sum($_) . " / " . sum($head) } @engines) .
      qq[ FROM ] .
      (join ", ", map { qq[(SELECT * FROM results WHERE ($bmcond) AND Threads = $threads AND `Param: engineName` = "$_") AS `$_` ]} @engines) . " WHERE " .
      (join " AND ", map {$paramEqualIfDefined->("Param: size") } ("Param: step", "Param: work", "Param: size", "Threads", "Benchmark") ) .
      qq[ GROUP BY `$head`.Benchmark];
    #say $queryString;
    my $row = $DBH->selectrow_arrayref($queryString);

    if (not $row) {
      say "bargraph for $name ($threads threads) did not return results";
      #say $queryString;
    }
    else {
      if ($MANUAL_BARGRAPH_HACK) {
        $row = [@$row, 1];
      }
      say $OUT join ", ", $name, @$row;
    }
  }
  close $OUT;
  qx[perl $BARGRAPH -pdf $TMPFILE > $threads$name.pdf ];
  unlink $TMPFILE;
  chdir "..";
}




##### IMPORTING

sub importCSV() {
  my @files;
  find(sub {
      push @files, $File::Find::name if $_ =~ /\.csv$/;
    }, $CSVDIR);
  for my $file (@files) {

    my @data = @{ csv(in => $file) };
    say "$file is empty" and next if !@data;
    my @headers = @{ shift @data };
    push @headers, "gitrev";
    updateTable($DBH, $TABLE, @headers);

    my $hash = $1 if ($file =~ m#/(\w{40})/#);

    for my $row (@data) {
      s/(?<=\d),(?=\d)/./g for @$row;  # replace , with . in numbers
    }
    my $sth = $DBH->prepare("INSERT INTO $TABLE (" . (join ",", map {qq["$_"]} @headers) . ") VALUES (" . (join ',', map {'?'} @headers) . ")");
    $sth->execute(@$_, $hash) for @data;
  }
  $DBH->do("UPDATE $TABLE SET Score = Score / 1000, Unit = 'ops/ms' WHERE Unit = 'ops/s'");
  $DBH->do("UPDATE $TABLE SET Samples = 1 WHERE Samples is NULL");
  $DBH->commit();
  return $DBH;
}

sub updateTable($DBH, $TABLE, @columns) {

  sub typeColumn($columnName) {
    given($columnName) {
      when(["Threads", "Score", 'Score Error (99,9%)', 'Samples', 'Param: depth', 'Param: sources']) { return qq["$columnName" REAL] }
      default { return qq["$columnName"] }
    }
  }

  if($DBH->selectrow_array("SELECT name FROM sqlite_master WHERE type='table' AND name='$TABLE'")) {
    my %knownColumns = map {$_ => 1} @{ $DBH->selectcol_arrayref("PRAGMA table_info($TABLE)", { Columns => [2]}) };
    @columns = grep {! defined $knownColumns{$_} } @columns;
    $DBH->do("ALTER TABLE $TABLE ADD COLUMN ". typeColumn($_) . " DEFAULT NULL") for @columns;
    return $DBH;
  }

  $DBH->do("CREATE TABLE $TABLE (" . (join ',', map { typeColumn($_) . " DEFAULT NULL" } @columns) . ')')
    or die "could not create table";
  return $DBH;
}
