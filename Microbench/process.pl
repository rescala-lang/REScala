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

# combining standard deviations is not trivial, but would be possible:
# http://www.burtonsys.com/climate/composite_standard_deviations.html

{
  my $dbPath = ':memory:';
  my $table = 'results';
  my $csvDir = 'resultStore';
  my $outDir = 'fig';

  my $dbh = DBI->connect("dbi:SQLite:dbname=". $dbPath,"","",{AutoCommit => 0,PrintError => 1});

  my @frameworks = qw( REScalaSync ParRP REScalaSTM ); #  scala.rx scala.react SIDUP;
  my @engines = ("synchron", "spinning", "stm");

  importCSV($csvDir, $dbh, $table);

  mkdir $outDir;
  chdir $outDir;

  plotBenchmarksFor($dbh, $table, "simple", "Simple Local State",
    map { {Title => "$_", "Param: riname" => $_, Benchmark => "benchmarks.simple.Mapping.local"} } @frameworks);
  plotBenchmarksFor($dbh, $table, "simple", "Simple Shared State",
    map { {Title => "$_", "Param: riname" => $_, Benchmark => "benchmarks.simple.Mapping.shared"} } @frameworks);
  for my $philosophers (32, 64, 256) {
    for my $layout (qw<alternating random third block>) {
      plotBenchmarksFor($dbh, $table, "philosophers$philosophers", $layout,
        map { {Title => $_, "Param: engineName" => $_ , Benchmark => "benchmarks.philosophers.PhilosopherCompetition.eat",
        "Param: philosophers" => $philosophers, "Param: layout" => $layout} } @engines);
    }
    for my $engine (@engines) {
      plotBenchmarksFor($dbh, $table, "philosophers$philosophers", "philosopher comparison $engine",
        map { {Title => $_, "Param: engineName" => $engine , Benchmark => "benchmarks.philosophers.PhilosopherCompetition.eat",
        "Param: philosophers" => $philosophers, "Param: layout" => $_} } qw<alternating random third block>);
    }
  }
  plotBenchmarksFor($dbh, $table, "philosophers", "Philosopher Table",
    map { {Title => $_, "Param: engineName" => $_ , Benchmark =>  "benchmarks.philosophers.PhilosopherCompetition.eat"} } @engines);

  plotBenchmarksFor($dbh, $table, "grid", "Prime Grid",
    map { {Title => $_, "Param: riname" => $_, Benchmark => "benchmarks.grid.Bench.primGrid" } } @frameworks);

  plotBenchmarksFor($dbh, $table, "stacks", "Dynamic",
    map {{Title => $_, "Param: work" => 0, "Param: engineName" => $_ , Benchmark => "benchmarks.dynamic.Stacks.run" }} @engines);

  # plotBenchmarksFor($dbh, $table, "philosophersBackoff", "backoff",
  #   map { {Title => $_,  "Param: engineName" => 'spinning' , Benchmark => "benchmarks.philosophers.PhilosopherCompetition.eat", "Param: spinningBackOff" => $_ } } (0..9) );

  my $query = queryDataset($dbh, query($table, "Param: work", "Benchmark", "Param: engineName"));
  plotDatasets("conflicts", "Asymmetric Workloads", {xlabel => "Work"},
    $query->("pessimistic cheap", "benchmarks.conflict.ExpensiveConflict.g:cheap", "spinning"),
    $query->("pessimistic expensive", "benchmarks.conflict.ExpensiveConflict.g:expensive", "spinning"),
    $query->("stm cheap", "benchmarks.conflict.ExpensiveConflict.g:cheap", "stm"),
    $query->("stm expensive", "benchmarks.conflict.ExpensiveConflict.g:expensive", "stm"));

  plotDatasets("conflicts", "STM aborts", {xlabel => "Work"},
    $query->("stm cheap", "benchmarks.conflict.ExpensiveConflict.g:cheap", "stm"),
    $query->("stm expensive", "benchmarks.conflict.ExpensiveConflict.g:expensive", "stm"),
    $query->("stm expensive tried", "benchmarks.conflict.ExpensiveConflict.g:tried", "stm"));

  for my $chance (0.01, 0.001, 0) {
    plotBenchmarksFor($dbh, $table, "stmbank", "Bank Accounts $chance",
      (map { {Title => $_, "Param: engineName" => $_, Benchmark => "benchmarks.STMBank.BankAccounts.reactive", "Param: globalReadChance" => $chance } } @engines),
      {Title => "pureSTM", Benchmark => "benchmarks.STMBank.BankAccounts.stm", "Param: globalReadChance" => $chance} );
  }
  $dbh->commit();
}

sub prettyName($name) {
  given ($name) {
    when (/spinning|REScalaSpin|ParRP/) { "ParRP" }
    when (/stm|REScalaSTM/) { "STM" }
    when (/synchron|REScalaSync/) { "Synchron" }
    default { $_ }
  }
}

sub query($tableName, $varying, @keys) {
  my $where = join " AND ", map {qq["$_" = ?]} @keys;
  return qq[SELECT "$varying", sum(Score * Samples) / sum(Samples) FROM "$tableName" WHERE $where GROUP BY "$varying" ORDER BY "$varying"];
}

sub plotBenchmarksFor($dbh, $tableName, $group, $name, @graphs) {
  my @datasets;
  for my $graph (@graphs) {
    my $title = delete $graph->{"Title"};
    my @keys = keys %{$graph};
    push @datasets, queryDataset($dbh, query($tableName, "Threads", @keys))->(prettyName($title) // "unnamed", values %{$graph});
  }
  plotDatasets($group, $name, {}, @datasets);
}

sub queryDataset($dbh, $query) {
  my $sth = $dbh->prepare($query);
  return sub($title, @params) {
    $sth->execute(@params);
    my $data = $sth->fetchall_arrayref();
    return makeDataset($title, $data) if (@$data);
    say "query for $title had no results: [$query] @params";
    return;
  }
}

sub makeDataset($title, $data) {
  $data = [sort {$a->[0] <=> $b->[0]} @$data];
  Chart::Gnuplot::DataSet->new(
    xdata => [map {$_->[0]} @$data],
    ydata => [map {$_->[1]} @$data],
    title => $title,
    style => "linespoints",
  );
}

sub plotDatasets($group, $name, $additionalParams, @datasets) {
  mkdir $group;
  unless (@datasets) {
    say "dataset for $group/$name is empty";
    return;
  }
  my $nospace = $name =~ s/\s//gr; # / highlighter
  my $chart = Chart::Gnuplot->new(
    output => "$group/$nospace.pdf",
    terminal => "pdf size 8,5 enhanced font 'Linux Libertine O,14'",
    key => "left top", #outside
    title  => $name,
    xlabel => "Threads",
    #logscale => "x 2; set logscale y 10",
    ylabel => "Operations Per Millisecond",
    %$additionalParams
  );
  $chart->plot2d(@datasets);
}

sub updateTable($dbh, $tableName, @columns) {

  sub typeColumn($columnName) {
    given($columnName) {
      when(["Threads", "Score", 'Score Error (99,9%)', 'Samples', 'Param: depth', 'Param: sources']) { return qq["$columnName" REAL] }
      default { return qq["$columnName"] }
    }
  }

  if($dbh->selectrow_array("SELECT name FROM sqlite_master WHERE type='table' AND name='$tableName'")) {
    my %knownColumns = map {$_ => 1} @{ $dbh->selectcol_arrayref("PRAGMA table_info($tableName)", { Columns => [2]}) };
    @columns = grep {! defined $knownColumns{$_} } @columns;
    $dbh->do("ALTER TABLE $tableName ADD COLUMN ". typeColumn($_) . " DEFAULT NULL") for @columns;
    return $dbh;
  }

  $dbh->do("CREATE TABLE $tableName (" . (join ',', map { typeColumn($_) . " DEFAULT NULL" } @columns) . ')')
    or die "could not create table";
  return $dbh;
}

sub importCSV($folder, $dbh, $tableName) {
  my @files;
  find(sub {
      push @files, $File::Find::name if $_ =~ /\.csv$/;
    }, $folder);
  for my $file (@files) {
    my @data = @{ csv(in => $file) };
    say "$file is empty" and next if !@data;
    my @headers = @{ shift @data };
    updateTable($dbh, $tableName, @headers);

    for my $row (@data) {
      s/(?<=\d),(?=\d)/./g for @$row;  # replace , with . in numbers
    }
    my $sth = $dbh->prepare("INSERT INTO $tableName (" . (join ",", map {qq["$_"]} @headers) . ") VALUES (" . (join ',', map {'?'} @headers) . ")");
    $sth->execute(@$_) for @data;
  }
  $dbh->do("UPDATE $tableName SET Score = Score / 1000, Unit = 'ops/ms' WHERE Unit = 'ops/s'");
  $dbh->commit();
  return $dbh;
}
