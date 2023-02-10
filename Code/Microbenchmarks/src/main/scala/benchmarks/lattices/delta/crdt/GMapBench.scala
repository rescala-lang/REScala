package benchmarks.lattices.delta.crdt

import kofre.datatypes.{EnableWinsFlag, GrowOnlyMap}
import kofre.datatypes.GrowOnlyMap.given
import org.openjdk.jmh.annotations.*

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

  type Contained = GrowOnlyMap[Int, EnableWinsFlag]
  type SUT       = DeltaBufferDotted[Contained]
  var map: SUT = _

  @Setup
  def setup(): Unit = {

    map = (0 until numEntries).foldLeft(NamedDeltaBuffer.dotted("a", GrowOnlyMap.empty[Int, EnableWinsFlag]): SUT) {
      case (rdc: SUT, i) =>
        rdc.mutateKeyNamedCtx(i, EnableWinsFlag.empty)(_.enable())
    }
  }

  @Benchmark
  def queryExisting() = map.queryKey(0).map(_.read)

  @Benchmark
  def queryMissing() = map.queryKey(-1).map(_.read)

  @Benchmark
  def containsExisting() = map.contains(0)

  @Benchmark
  def containsMissing() = map.contains(-1)

  @Benchmark
  def queryAllEntries(): Iterable[Boolean] = map.queryAllEntries().map(_.read)

  @Benchmark
  def mutateExisting(): SUT = map.mutateKeyNamedCtx(0, EnableWinsFlag.empty)(_.disable())

  @Benchmark
  def mutateMissing(): SUT =
    map.mutateKeyNamedCtx(0, EnableWinsFlag.empty)(_.enable())
}
