package benchmarks.lattices.delta.crdt

import kofre.causality.CausalContext
import kofre.contextual.WithContextDecompose.DotSet
import kofre.decompose.interfaces.EnableWinsFlag.EnableWinsFlagOps
import kofre.decompose.interfaces.ORMapInterface.{ORMap, ORMapSyntax}
import org.openjdk.jmh.annotations._
import kofre.decompose.containers.DeltaBufferRDT

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

  type SUT = DeltaBufferRDT[ORMap[Int, CausalContext]]

  var map: SUT = _

  @Setup
  def setup(): Unit = {
    map = (0 until numEntries).foldLeft(DeltaBufferRDT[ORMap[Int, CausalContext]]("a")) {
      case (m, i) => new ORMapSyntax(m).mutateKeyNamedCtx(i)(_.enable())
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
  def mutateMissing(): SUT = map.mutateKeyNamedCtx(-1)(_.enable())

  @Benchmark
  def removeExisting(): SUT = map.remove(0)

  @Benchmark
  def removeMissing(): SUT = map.remove(-1)

  @Benchmark
  def removeAll(): SUT = map.removeAll(0 until numEntries)

  @Benchmark
  def removeByValue(): SUT = map.removeByValue(_.read)

  @Benchmark
  def clear(): SUT = map.clear()
}
