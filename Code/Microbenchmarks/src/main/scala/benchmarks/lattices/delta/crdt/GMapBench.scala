package benchmarks.lattices.delta.crdt

import kofre.base.DecomposeLattice
import kofre.contextual.{ContextDecompose, ContextLattice}
import kofre.decompose.containers.DeltaBufferRDT
import kofre.decompose.interfaces.{EnableWinsFlag, GMapInterface}
import kofre.decompose.interfaces.GMapInterface.GMap
import kofre.decompose.interfaces.GMapInterface.GMapSyntax
import org.openjdk.jmh.annotations._

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



  type Contained = GMap[Int, EnableWinsFlag]
  type SUT = DeltaBufferRDT[Contained]
  var map: SUT = _

  @Setup
  def setup(): Unit = {
    map = (0 until numEntries).foldLeft(DeltaBufferRDT.empty("a", GMapInterface.empty[Int, EnableWinsFlag]): SUT) {
      case (rdc: SUT, i) =>
        rdc.mutateKeyNamedCtx(i)(_.enable())(DeltaBufferRDT.contextPermissions, DeltaBufferRDT.contextPermissions, implicitly[ContextDecompose[EnableWinsFlag]])
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
  def mutateExisting(): SUT = map.mutateKeyNamedCtx(0)(_.disable())

  @Benchmark
  def mutateMissing(): SUT =
    map.mutateKeyNamedCtx(0)(_.enable())
}
