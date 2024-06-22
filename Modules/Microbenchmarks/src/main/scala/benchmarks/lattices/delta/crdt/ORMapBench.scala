package benchmarks.lattices.delta.crdt

import org.openjdk.jmh.annotations.*
import rdts.base.Bottom
import rdts.datatypes.contextual.{EnableWinsFlag, ObserveRemoveMap}
import rdts.dotted.Dotted

import java.util.concurrent.TimeUnit

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class ORMapBench {

  @Param(Array("1", "10", "100", "1000"))
  var numEntries: Int = scala.compiletime.uninitialized

  type SUT = DeltaBufferDotted[ObserveRemoveMap[Int, EnableWinsFlag]]

  var map: SUT = scala.compiletime.uninitialized

  @Setup
  def setup(): Unit = {
    map = (0 until numEntries).foldLeft(NamedDeltaBuffer.dotted[ObserveRemoveMap[Int, EnableWinsFlag]](
      "a",
      ObserveRemoveMap.empty
    )) {
      case (m, i) => m.mod(_.transform(i)(_.mod(_.enable(using m.replicaID)())))
    }
  }

  @Benchmark
  def queryExisting(): Boolean = map.data.queryKey(0).read

  @Benchmark
  def queryMissing(): Boolean = map.data.queryKey(-1).read

  @Benchmark
  def containsExisting(): Boolean = map.data.contains(0)

  @Benchmark
  def containsMissing(): Boolean = map.data.contains(-1)

  @Benchmark
  def queryAllEntries(): Iterable[Boolean] = map.data.queryAllEntries.map(_.read)

  @Benchmark
  def mutateExisting(): SUT = map.mod(_.transform(0)(_.mod(_.disable())))

  @Benchmark
  def mutateMissing(): SUT = map.mod(_.transform(-1)(_.mod(_.enable(using map.replicaID)())))

  @Benchmark
  def removeExisting(): SUT = map.mod(_.remove(0))

  @Benchmark
  def removeMissing(): SUT = map.mod(_.remove(-1))

  @Benchmark
  def removeAll(): SUT = map.mod(_.removeAll(0 until numEntries))

  @Benchmark
  def removeByValue(): SUT = map.mod(_.removeByValue(ewf => ewf.data.read))

  @Benchmark
  def clear(): SUT = map.mod(_.clear())
}
