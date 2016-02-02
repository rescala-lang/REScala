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

our $NAME_FINE = "Manual";
our $NAME_COARSE = "G-Lock";

our $LEGEND_POS = "left top";
our $YRANGE = "[0:]";
our $GNUPLOT_TERMINAL = "pdf size 15,9";

my $DBH = DBI->connect("dbi:SQLite:dbname=". $DBPATH,"","",{AutoCommit => 0,PrintError => 1});
{

  importCSV();
  $DBH->do("DELETE FROM $TABLE WHERE Threads > 16");
  $DBH->do(qq[DELETE FROM $TABLE WHERE "Param: engineName" = "fair"]);

  remove_tree($_) for glob("$OUTDIR/*");
  mkdir $OUTDIR;
  chdir $OUTDIR;

  for my $dynamic (queryChoices("Param: tableType")) {
    for my $philosophers (queryChoices("Param: philosophers", "Param: tableType" => $dynamic)) {
      local $LEGEND_POS = "left top" if $philosophers == 48  || $philosophers == 16;
      for my $layout (queryChoices("Param: layout", "Param: tableType" => $dynamic, "Param: philosophers" => $philosophers)) {
        local $YRANGE = "[0:500]" if $philosophers <= 64 && $dynamic eq "static";
        local $YRANGE = "[0:300]" if $philosophers <= 32 && $dynamic eq "static";
        local $YRANGE = "[0:800]" if $philosophers > 64 && $dynamic eq "static";
        local $YRANGE = "[0:300]" if $dynamic ne "static";
        local $YRANGE = "[0:160]" if $dynamic ne "static" && $philosophers <= 32;
        local $YRANGE = "[0:]" if $layout eq "third";
        local $LEGEND_POS = "left top" if $layout eq "third";
        local $NAME_FINE = "No Sync" if $layout eq "third";
        plotBenchmarksFor("${dynamic}philosophers$philosophers", $layout,
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
    plotBenchmarksFor("${dynamic}philosophers", "philosopher comparison engine scaling",
      map { $byPhilosopher->($_) } (queryChoices("Param: engineName", "Param: tableType" => $dynamic)));


    plotBenchmarksFor("${dynamic}philosophers", "Philosopher Table",
      map { {Title => $_, "Param: engineName" => $_ , Benchmark =>  "benchmarks.philosophers.PhilosopherCompetition.eat", "Param: tableType" => $dynamic } }  queryChoices("Param: engineName", "Param: tableType" => $dynamic));

    { # varying conflict potential
      my $query = queryDataset(query("Param: philosophers", "Benchmark", "Param: engineName", "Param: tableType", "Threads"));
      plotDatasets("${dynamic}philosophers", "Concurrency Scaling", {xlabel => "Philosophers"},
        $query->("ParRP", "benchmarks.philosophers.PhilosopherCompetition.eat", "parrp", $dynamic, 16),
        $query->("STM", "benchmarks.philosophers.PhilosopherCompetition.eat", "stm", $dynamic, 16),
        $query->("Synchron", "benchmarks.philosophers.PhilosopherCompetition.eat", "synchron", $dynamic, 16));
    }

  }

  # plotChoices("backoff", "dynamic10", "Param: factorBackoff", "Param: minBackoff" => 10000,"Param: maxBackoff" => 1000000,  "Param: engineName" => "parrp" , Benchmark => "benchmarks.philosophers.PhilosopherCompetition.eat",
  #         "Param: philosophers" => 48, "Param: layout" => "alternating", "Param: tableType" => "dynamic" );
  # plotChoices("backoff", "dynamic100", "Param: factorBackoff", "Param: minBackoff" => 100000,"Param: maxBackoff" => 1000000,  "Param: engineName" => "parrp" , Benchmark => "benchmarks.philosophers.PhilosopherCompetition.eat",
  #         "Param: philosophers" => 48, "Param: layout" => "alternating", "Param: tableType" => "dynamic" );
  # plotChoices("backoff", "dynamic1000", "Param: factorBackoff", "Param: minBackoff" => 1000000,"Param: maxBackoff" => 1000000,  "Param: engineName" => "parrp" , Benchmark => "benchmarks.philosophers.PhilosopherCompetition.eat",
  #         "Param: philosophers" => 48, "Param: layout" => "alternating", "Param: tableType" => "dynamic" );

  {
    my $BMCOND = qq[(results.Benchmark like "benchmarks.simple.SimplePhil%"
                  OR results.Benchmark = "benchmarks.simple.TurnCreation.run"
                  OR results.Benchmark = "benchmarks.simple.NaturalGraph.run"
                  OR (results.Benchmark = "benchmarks.philosophers.PhilosopherCompetition.eat"
                   AND `Param: tableType` = "static" AND `Param: layout` = "alternating"))];
    my $res = $DBH->selectall_arrayref(qq[SELECT parrp.Benchmark, sync.Score/sync.Score , parrp.Score/ sync.Score, stm.Score / sync.Score from
      (SELECT * from results where $BMCOND and Threads = 1 and `Param: engineName` = "parrp") AS parrp,
      (SELECT * from results where $BMCOND and Threads = 1 and `Param: engineName` = "synchron") AS sync,
      (SELECT * from results where $BMCOND and Threads = 1 and `Param: engineName` = "stm") AS stm
      WHERE sync.Benchmark = parrp.Benchmark AND sync.Benchmark = stm.Benchmark
      AND sync.Threads = parrp.Threads AND stm.Threads = sync.Threads
      AND (sync.`Param: step` IS NULL OR (parrp.`Param: step` = sync.`Param: step` AND sync.`Param: step` = stm.`Param: step`))
      AND (sync.`Param: work` IS NULL OR (parrp.`Param: work` = sync.`Param: work` AND sync.`Param: work` = stm.`Param: work`))]);
    my $TMPFILE = "out.perf";
    open my $OUT, ">", $TMPFILE;
    say $OUT "=cluster $NAME_COARSE ParRP STM
=sortbmarks
yformat=%1.1f
xlabel=Benchmark
ylabel=Speedup compared to $NAME_COARSE
colors=red,green,blue
=table,";
    for my $row (@$res) {
      $row->[0] = unmangleName($row->[0]);
      $row->[0] =~ s/benchmarks.simple.(?:Creation.|SimplePhil.)?([^\.]+)/$1/;
      $row->[0] =~ s/buildAndPropagate/build + propagate/;
      $row->[0] =~ s/benchmarks.philosophers.PhilosopherCompetition.eat/philosophers/;
      $row->[0] =~ s/TurnCreation.run/structures/;
      say $OUT join ", ", @$row;
    }
    close $OUT;
    qx[perl $BARGRAPH -pdf $TMPFILE > serialBenchmarks.pdf ];
    unlink $TMPFILE;
  }


  plotBenchmarksFor("stacks", "Dynamic",
    map {{Title => $_, "Param: work" => 0, "Param: engineName" => $_ , Benchmark => "benchmarks.dynamic.Stacks.run" }}
      queryChoices("Param: engineName", Benchmark => "benchmarks.dynamic.Stacks.run"));

  { # simplePhil
    local $NAME_FINE = "No Sync";
    for my $bench (qw< propagate build buildAndPropagate >) {
      local $LEGEND_POS = "left top" if $bench eq "build";
      plotBenchmarksFor("simplePhil", "SimplePhil$bench",
        map {{Title => $_, "Param: engineName" => $_ , Benchmark => "benchmarks.simple.SimplePhil.$bench" }}
          queryChoices("Param: engineName", Benchmark => "benchmarks.simple.SimplePhil.$bench"));
    }
  }


  { # expensive conflict stuff
    my $query = queryDataset(query("Param: work", "Benchmark", "Param: engineName"));
    plotDatasets("conflicts", "Asymmetric Workloads", {xlabel => "Work"},
      $query->("pessimistic cheap", "benchmarks.conflict.ExpensiveConflict.g:cheap", "parrp"),
      $query->("pessimistic expensive", "benchmarks.conflict.ExpensiveConflict.g:expensive", "parrp"),
      $query->("stm cheap", "benchmarks.conflict.ExpensiveConflict.g:cheap", "stm"),
      $query->("stm expensive", "benchmarks.conflict.ExpensiveConflict.g:expensive", "stm"));

    plotDatasets("conflicts", "STM aborts", {xlabel => "Work"},
      $query->("stm cheap", "benchmarks.conflict.ExpensiveConflict.g:cheap", "stm"),
      $query->("stm expensive", "benchmarks.conflict.ExpensiveConflict.g:expensive", "stm"),
      $query->("stm expensive tried", "benchmarks.conflict.ExpensiveConflict.g:tried", "stm"));
  }

  for my $benchmark (grep {/Creation|SingleVar/} queryChoices("Benchmark")) {
    plotBenchmarksFor("other", $benchmark,
      map {{Title => $_, "Param: engineName" => $_ , Benchmark => $benchmark }}
        queryChoices("Param: engineName", Benchmark => $benchmark));
  }


  { # chain, fan
    for my $benchmark (grep {/simple\.(Chain|Fan)/} queryChoices("Benchmark")) {
      my $query = queryDataset(query("Param: size", "Benchmark", "Param: engineName"));
      plotDatasets("simple", $benchmark, {xlabel => "Size", logscale => "x 10",},
        map { $query->(prettyName($_), $benchmark, $_) } queryChoices("Param: engineName", "Benchmark" => $benchmark));
    }
  }

  { # stmbank
    for my $globalReadChance (queryChoices("Param: globalReadChance")) {
      my $benchmark = "benchmarks.STMBank.BankAccounts.reactive";
      plotBenchmarksFor("BankAccounts", $globalReadChance,
        (map {{Title => $_, "Param: globalReadChance" => $globalReadChance, "Param: engineName" => $_ , Benchmark => $benchmark }}
          queryChoices("Param: engineName", Benchmark => $benchmark, "Param: globalReadChance" => $globalReadChance)),
        {Title => "Pure STM", "Param: globalReadChance" => $globalReadChance, Benchmark => "benchmarks.STMBank.BankAccounts.stm"});
    }
  }

  { # reverse fan
      my $benchmark = "benchmarks.simple.ReverseFan.run";
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
      plotBenchmarksFor("ChatServer", "$rooms",
        (map {{Title => $_, "Param: engineName" => $_ , Benchmark => $benchmark, "Param: size" => $rooms }}
          queryChoices("Param: engineName", Benchmark => $benchmark, "Param: size" => $rooms)),);
    }
  }


  {#universe
    plotBenchmarksFor("Universe", "Universe",
      (map {{Title => $_, "Param: engineName" => $_ , Benchmark => "UniverseCaseStudy" }}
          queryChoices("Param: engineName", Benchmark => "UniverseCaseStudy")));
  }

  $DBH->commit();
}

sub prettyName($name) {
  $name =~  s/pessimistic|spinning|REScalaSpin|parrp/ParRP/;
  $name =~  s/stm|REScalaSTM/STM/;
  $name =~  s/synchron|REScalaSync/$NAME_COARSE/;
  $name =~  s/unmanaged/$NAME_FINE/;
  return $name;
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
    push @datasets, queryDataset(query("Threads", @keys))->(prettyName($title) // "unnamed", values %{$graph});
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


sub styleByName($name) {
  given($name) {
    when (/ParRP/)    { 'linecolor "dark-green" lt 2 lw 2 pt 7 ps 1' }
    when (/STM/)      { 'linecolor "blue" lt 2 lw 2 pt 5 ps 1' }
    when (/$NAME_COARSE/) { 'linecolor "red" lt 2 lw 2 pt 9 ps 1' }
    when (/fair/)     { 'linecolor "light-blue" lt 2 lw 2 pt 8 ps 1' }
    when (/$NAME_FINE/)   { 'linecolor "black" lt 2 lw 2 pt 11 ps 1' }
    default { '' }
  }
}
sub styling($name) {
  my $res = styleByName($name);
  if ($name =~ /(\d+)/) {
    my $pt = $1;
    $res =~ s/pt \d+/pt $pt/;
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

sub plotDatasets($group, $name, $additionalParams, @datasets) {
  mkdir $group;
  unless (@datasets) {
    say "dataset for $group/$name is empty";
    return;
  }
  $name = unmangleName($name);
  my $nospecial = $name =~ s/\W/_/gr; # / highlighter
  my $chart = Chart::Gnuplot->new(
    output => "$group/$nospecial.pdf",
    terminal => "$GNUPLOT_TERMINAL enhanced font 'Linux Libertine O,30'",
    key => $LEGEND_POS,
    #title  => $name,
    #xlabel => "Active threads",
    yrange => $YRANGE,
    #logscale => "x 2; set logscale y 10",
    #ylabel => "Operations per millisecond",
    # xrange => "reverse",
    lmargin => 4.8,
    rmargin => 1.5,
    tmargin => 0.3,
    bmargin => 1.5,
    %$additionalParams
  );
  $chart->plot2d(@datasets);
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
    updateTable($DBH, $TABLE, @headers);

    for my $row (@data) {
      s/(?<=\d),(?=\d)/./g for @$row;  # replace , with . in numbers
    }
    my $sth = $DBH->prepare("INSERT INTO $TABLE (" . (join ",", map {qq["$_"]} @headers) . ") VALUES (" . (join ',', map {'?'} @headers) . ")");
    $sth->execute(@$_) for @data;
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
