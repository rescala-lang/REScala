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
use List::Util qw(max);
use Cwd 'abs_path';

# combining standard deviations is not trivial, but would be possible:
# http://www.burtonsys.com/climate/composite_standard_deviations.html

my $DBPATH = ':memory:';
my $TABLE = 'results';
my $CSVDIR = 'resultStore';
my $OUTDIR = 'fig';

our $FONT = "Times New Roman";
our $FONTSIZE = "30";

our $NAME_FINE = "Handcrafted";
our $NAME_COARSE = "G-Lock";
our $NAME_FULLMV = "FullMV";
our $NAME_PARRP = "MV-RP";
our $NAME_STM = "STM-RP";
our $NAME_RESTORING = "Snapshots";

our $YTIC_COUNT = 4;
our $YRANGE_ROUND = 100;
our $LEGEND_POS = "off";
our $YMAX = 0;
our $XRANGE = "[:]";
our $GNUPLOT_TERMINAL = "pdf size 5,2.5";
our %MARGINS = (
    lmargin => 5.8,
    rmargin => 1.5,
    tmargin => 0.5,
    bmargin => 1.5,
  );
our $VERTICAL_LINE = undef;
our $X_VARYING = "Threads";
our %ADDITIONAL_GNUPLOT_PARAMS = ();

sub prettyName($name) {
  $name =~ s/Param: engineName:\s*//;
  $name =~ s/pessimistic|spinning|REScalaSpin|parrp/$NAME_PARRP/;
  $name =~ s/fullmv/$NAME_FULLMV/;
  $name =~ s/stm|REScalaSTM|STM/$NAME_STM/;
  $name =~ s/synchron|REScalaSynchron/$NAME_COARSE/;
  $name =~ s/unmanaged/$NAME_FINE/;
  $name =~ s/restoring/$NAME_RESTORING/;
  return $name;
}

sub styleByName($name) {
  given($name) {
    when (/$NAME_PARRP/)     { 'linecolor "dark-green" lt 2 lw 2 pt 7  ps 1' }
    when (/$NAME_STM/)       { 'linecolor "dark-green"       lt 2 lw 2 pt 7  ps 1' }
    when (/$NAME_COARSE|Restore/)    { 'linecolor "red"       lt 2 lw 2 pt 9  ps 1' }
    when (/fair/)            { 'linecolor "light-blue" lt 2 lw 2 pt 8  ps 1' }
    when (/$NAME_FULLMV/) { 'linecolor "blue" lt 2 lw 2 pt 5  ps 1' }
    when (/$NAME_FINE/)      { 'linecolor "black"      lt 2 lw 2 pt 11 ps 1' }
    when (/$NAME_RESTORING|Derive/) { 'linecolor "dark-green" lt 2 lw 2 pt 6  ps 1' }
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

  distributionBenchmarks();

  $DBH->commit();
}

sub distributionBenchmarks() {
  { # conflict distance
    my $benchmark1 = "rescala.benchmarks.distributed.rtt.ConflictDistances.run";
    my $benchmark2 = "rescala.benchmarks.distributed.rtt.Remerge.run";
    my $query = queryDataset(query("Param: mergeAt", "Benchmark", "Param: totalLength"));
    local $XRANGE = "[1:]";
    {
      local $YTIC_COUNT = 5;
      local $YMAX = 5500;
    
      plotDatasets("rtt", "conflictdistance-total", {xlabel => "Merge Depth"},
        map { $query->("$_ Hops", $benchmark1, $_) } (reverse queryChoices("Param: totalLength", "Benchmark" => $benchmark1)) 
      );
      
      plotDatasets("rtt", "remerge-total", {xlabel => "Merge Depth"},
        map { $query->("$_ Hops", $benchmark2, $_) } (reverse queryChoices("Param: totalLength", "Benchmark" => $benchmark2)) 
      );
    }
    
    {
      my @datasets = map {
        my $name = prettyName(unmangleName($_));
        Chart::Gnuplot::DataSet->new(
          xdata => [10,20],
          ydata => [10,20],
          title => $name,
          style => 'linespoints ' . styling($name),
        )
        } reverse(queryChoices("Param: totalLength"));
      local %MARGINS = (
          lmargin => 0.5,
          rmargin => 0.5,
          tmargin => 0.2,
          bmargin => 0.2,
        );
      my $chart = Chart::Gnuplot->new(
        output => "rtt/legend.pdf",
        terminal => "pdf size 8,.75 enhanced font '$FONT,$FONTSIZE'",
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
    
    
    my $q = queryDataset(query("avg(score)", "Benchmark", "Param: depthHosts"));
    for my $depth (queryChoices("Param: totalLength", "Benchmark" => "rescala.benchmarks.distributed.rtt.ConflictDistances.run")) {
      my $baseline = shift ($q->("$depth Hosts", "rescala.benchmarks.distributed.rtt.DistributedSignalMapGrid.run", $depth)->ydata);
      say Dumper("Correcting Conflicts with totalLength $depth by baseline score of $baseline.");
      $DBH->do(qq[UPDATE $TABLE SET Score = Score - $baseline WHERE (Benchmark = "rescala.benchmarks.distributed.rtt.ConflictDistances.run" OR Benchmark = "rescala.benchmarks.distributed.rtt.Remerge.run") AND "Param: totalLength" = "$depth"]);
      $DBH->commit();
    }
    
    plotDatasets("rtt", "conflictdistance-relative", {xlabel => "Merge Depth"},
      map { $query->("$_ Hops", $benchmark1, $_) } (reverse queryChoices("Param: totalLength", "Benchmark" => $benchmark1)) 
    );
    
    plotDatasets("rtt", "remerge-relative", {xlabel => "Merge Depth"},
      map { $query->("$_ Hops", $benchmark2, $_) } (reverse queryChoices("Param: totalLength", "Benchmark" => $benchmark2)) 
    );
    
    # say Dumper(queryDataset(query("Param: totalLength", "Benchmark", "Param: mergeAt")) -> ("before", "rescala.benchmarks.distributed.rtt.ConflictDistances.run", "3"));
    # say Dumper(queryChoices("Param: totalLength", "Benchmark" => $benchmark1));
    
    say Dumper("Re-orienting stuff");
    $DBH->do(qq[UPDATE $TABLE SET "Param: totalLength" = "Param: totalLength" - "Param: mergeAt" WHERE Benchmark = "rescala.benchmarks.distributed.rtt.ConflictDistances.run" OR Benchmark = "rescala.benchmarks.distributed.rtt.Remerge.run"]);
    $DBH->commit();

    local $LEGEND_POS = "outside right center";
    local $GNUPLOT_TERMINAL = "pdf size 7,2.5";
    local %MARGINS = (
      lmargin => 5.8,
      rmargin => 20,
      tmargin => 0.5,
      bmargin => 1.5,
    );

    plotDatasets("rtt", "conflictdistance-bwd-relative", {xlabel => "Merge Depth"},
      map { $query->("$_", $benchmark1, $_) } (queryChoices("Param: totalLength", "Benchmark" => $benchmark1)) 
    );
    
    plotDatasets("rtt", "remerge-bwd-relative", {xlabel => "Merge Depth"},
      map { $query->("$_", $benchmark2, $_) } (queryChoices("Param: totalLength", "Benchmark" => $benchmark2)) 
    );
  }


  { # loop back
    local $LEGEND_POS = "inside right bottom";
    my $benchmark1 = "rescala.benchmarks.distributed.rtt.RemoteGlitchLoop.run";
    my $query1 = queryDataset(query("Threads", "Benchmark"));
    my $benchmark2 = "rescala.benchmarks.distributed.rtt.DistributedSignalMapGrid.run";
    my $query2 = queryDataset(query("Threads", "Benchmark", "Param: depthHosts"));
    plotDatasets("rtt", "loopback", {xlabel => "Threads"},
      $query2->("Chain", $benchmark2, 2),
      $query1->("Loop", $benchmark1),
    );
  }
  
  { # map grid single threaded
    local $LEGEND_POS = "inside left top";
    my $benchmark = "rescala.benchmarks.distributed.rtt.DistributedSignalMapGrid.run";
    my $query1 = queryDataset(query("Param: depthHosts", "Threads", "Benchmark", "Param: widthHosts"));
    my $query2 = queryDataset(query("Param: widthHosts", "Threads", "Benchmark", "Param: depthHosts"));
    plotDatasets("mapgrid", "singlethreaded", {xlabel => "Hops"},
      $query1->("1 Branch", 1, $benchmark, 1),
      $query2->("5 Hops", 1, $benchmark, 5),
    );
  }
  
  { # map grid pipelining
    local $LEGEND_POS = "outside right center";
    local $GNUPLOT_TERMINAL = "pdf size 9,2.5";
    local %MARGINS = (
      lmargin => 5.8,
      rmargin => 30,
      tmargin => 0.5,
      bmargin => 1.5,
    );
    my $benchmark = "rescala.benchmarks.distributed.rtt.DistributedSignalMapGrid.run";
    my $query = queryDataset(query("Threads", "Benchmark", "Param: depthHosts"));
    plotDatasets("mapgrid", "pipelining", {xlabel => "Threads"},
      map { $query->("$_ Hops", $benchmark, $_) } (reverse queryChoices("Param: depthHosts", "Benchmark" => $benchmark))
    );
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

sub roundTo($target, $value) {
  int($value / $target +1)* $target;
}

sub plotDatasets($group, $name, $additionalParams, @datasets) {
  mkdir $group;
  unless (@datasets) {
    say "dataset for $group/$name is empty";
    return;
  }
  # say Dumper(@datasets);
  my ($ymax,) = sort {$b <=> $a} map {@{$_->{ydata}}} @datasets;
  $ymax = max roundTo($YRANGE_ROUND, $ymax), $YMAX;
  $name = unmangleName($name);
  my $nospecial = $name =~ s/\W/_/gr; # / highlighter
  my $chart = Chart::Gnuplot->new(
    output => "$group/$nospecial.pdf",
    terminal => "$GNUPLOT_TERMINAL enhanced font '$FONT,$FONTSIZE'",
    key => $LEGEND_POS,
    #title  => $name,
    #xlabel => "Active threads",
    xrange => $XRANGE,
    yrange => ($YMAX eq 0) ? "[0:]" : "[0:$YMAX]",
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
      when(["Threads", "Score", 'Score Error (99,9%)', 'Samples', 'Param: totalLength', 'Param: mergeAt', 'Param: depth', 'Param: sources']) { return qq["$columnName" REAL] }
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
