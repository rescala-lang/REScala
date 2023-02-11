package benchmarks.lattices.delta.crdt

import kofre.datatypes.{EnableWinsFlag, ObserveRemoveMap}
import kofre.datatypes.ObserveRemoveMap
import org.openjdk.jmh.annotations.*

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
  var numEntries: Int = _

  type SUT = DeltaBufferDotted[ObserveRemoveMap[Int, EnableWinsFlag]]

  var map: SUT = _

  @Setup
  def setup(): Unit = {
    map = (0 until numEntries).foldLeft(NamedDeltaBuffer.dotted[ObserveRemoveMap[Int, EnableWinsFlag]](
      "a",
      ObserveRemoveMap.empty
    )) {
      case (m, i) => m.mutateKeyNamedCtx(i)(_.enable(using m.replicaID)())
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
  def queryAllEntries(): Iterable[Boolean] = map.queryAllEntries.map(_.read)

  @Benchmark
  def mutateExisting(): SUT = map.mutateKeyNamedCtx(0)(_.disable())

  @Benchmark
  def mutateMissing(): SUT = map.mutateKeyNamedCtx(-1)(_.enable(using map.replicaID)())

  @Benchmark
  def removeExisting(): SUT = map.remove(0)

  @Benchmark
  def removeMissing(): SUT = map.remove(-1)

  @Benchmark
  def removeAll(): SUT = map.removeAll(0 until numEntries)

  @Benchmark
  def removeByValue(): SUT = map.removeByValue(ewf => ewf.read)

  @Benchmark
  def clear(): SUT = map.clear()
}
