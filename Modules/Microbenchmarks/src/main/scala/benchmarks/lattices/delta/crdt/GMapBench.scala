package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.datatypes.GrowOnlyMap
import rdts.datatypes.GrowOnlyMap.given
import rdts.datatypes.contextual.EnableWinsFlag
import rdts.datatypes.GrowOnlyMap.mutateKeyNamedCtx


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
  var numEntries: Int = scala.compiletime.uninitialized

  type Contained = GrowOnlyMap[Int, EnableWinsFlag]
  type SUT       = DeltaBufferDotted[Contained]
  var map: SUT = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {

    map = (0 until numEntries).foldLeft(NamedDeltaBuffer.dotted("a", GrowOnlyMap.empty[Int, EnableWinsFlag]): SUT) {
      case (rdc: SUT, i) =>
        rdc.mod(_.mutateKeyNamedCtx(i, EnableWinsFlag.empty)(_.enable(using rdc.replicaID)()))
    }
  }

  @Benchmark
  def queryExisting() = map.data.get(0).map(_.read)

  @Benchmark
  def queryMissing() = map.data.get(-1).map(_.read)

  @Benchmark
  def containsExisting() = map.data.contains(0)

  @Benchmark
  def containsMissing() = map.data.contains(-1)

  @Benchmark
  def queryAllEntries(): Iterable[Boolean] = map.data.values.map(_.read)

  @Benchmark
  def mutateExisting(): SUT = map.mod(_.mutateKeyNamedCtx(0, EnableWinsFlag.empty)(_.disable()))

  @Benchmark
  def mutateMissing(): SUT =
    map.mod(_.mutateKeyNamedCtx(0, EnableWinsFlag.empty)(_.enable(using map.replicaID)()))
}
