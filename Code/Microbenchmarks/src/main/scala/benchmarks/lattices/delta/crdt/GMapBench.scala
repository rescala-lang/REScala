package benchmarks.lattices.delta.crdt

import kofre.decompose.interfaces.EWFlagInterface.{EWFlag, EWFlagSyntax}
import kofre.decompose.interfaces.GMapInterface.{GMap, GMapSyntax}
import org.openjdk.jmh.annotations._
import kofre.decompose.containers.ReactiveDeltaCRDT

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class GMapBench {

  @Param(Array("1", "10", "100", "1000"))
  var numEntries: Int = _

  type SUT = ReactiveDeltaCRDT[GMap[Int, EWFlag]]
  var map: SUT = _

  @Setup
  def setup(): Unit = {
    map = (0 until numEntries).foldLeft(ReactiveDeltaCRDT[GMap[Int, EWFlag]]("a")) {
      case (rdc, i) =>
        rdc.mutateKeyCtx(i)(arg => _.enable()(arg))
    }
  }

  @Benchmark
  def queryExisting(): Boolean = map.queryKey(0).read

  @Benchmark
  def queryMissing(): Boolean = map.queryKey(-1).read

  @Benchmark
  def containsExisting(): Boolean = map.contains(0)

  @Benchmark
  def containsMissing(): Boolean = map.contains(-1)

  @Benchmark
  def queryAllEntries(): Iterable[Boolean] = map.queryAllEntries().map(_.read)

  @Benchmark
  def mutateExisting(): SUT = map.mutateKeyCtx(0)((arg) => _.disable()(arg))

  @Benchmark
  def mutateMissing(): SUT = map.mutateKeyCtx(0)((arg) => _.enable()(arg))
}
