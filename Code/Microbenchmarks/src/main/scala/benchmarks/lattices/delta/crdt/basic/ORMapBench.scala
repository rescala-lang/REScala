package benchmarks.lattices.delta.crdt.basic

import org.openjdk.jmh.annotations._
import rescala.extra.lattices.delta.CContext.DietMapCContext
import rescala.extra.lattices.delta.crdt.reactive.{EWFlag, ORMap}
import rescala.extra.lattices.delta.interfaces.EWFlagInterface

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

  type SUT = ORMap[Int, EWFlag.Embedded, DietMapCContext]

  var map: SUT = _

  @Setup
  def setup(): Unit = {
    map = (0 until numEntries).foldLeft(ORMap[Int, EWFlag.Embedded, DietMapCContext]("a")) {
      case (m, i) => m.mutateKey(i, EWFlagInterface.enable())
    }
  }

  @Benchmark
  def queryExisting(): Boolean = map.queryKey(0, EWFlagInterface.read)

  @Benchmark
  def queryMissing(): Boolean = map.queryKey(-1, EWFlagInterface.read)

  @Benchmark
  def containsExisting(): Boolean = map.contains(0)

  @Benchmark
  def containsMissing(): Boolean = map.contains(-1)

  @Benchmark
  def queryAllEntries(): Iterable[Boolean] = map.queryAllEntries(EWFlagInterface.read)

  @Benchmark
  def mutateExisting(): SUT = map.mutateKey(0, EWFlagInterface.disable())

  @Benchmark
  def mutateMissing(): SUT = map.mutateKey(-1, EWFlagInterface.enable())

  @Benchmark
  def removeExisting(): SUT = map.remove(0)

  @Benchmark
  def removeMissing(): SUT = map.remove(-1)

  @Benchmark
  def removeAll(): SUT = map.removeAll(0 until numEntries)

  @Benchmark
  def removeByValue(): SUT = map.removeByValue(EWFlagInterface.read)

  @Benchmark
  def clear(): SUT = map.clear()
}
