package benchmarks.lattices

import java.util.concurrent.TimeUnit
import benchmarks.lattices.Codecs._
import org.openjdk.jmh.annotations._
import kofre.sets.AddWinsSetO
import kofre.{IdUtil, Lattice}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class AddWinsSetOBench {

  @Param(Array("1", "1000"))
  var setSize: Int = _

  var rep1Set: AddWinsSetO[String]        = _
  var rep1SetPlusOne: AddWinsSetO[String] = _
  var rep2Set: AddWinsSetO[String]        = _
  val rep1id                              = "rep1"
  val rep2id                              = "rep2"
  var rep2Delta: AddWinsSetO[String]      = _
  var justSetRep1: Set[String]            = _
  var justSetRep2: Set[String]            = _

  private def makeRep(rep: IdUtil.Id): AddWinsSetO[String] = {
    0.until(setSize).foldLeft(AddWinsSetO.empty[String]) { case (s, v) => s.add(v.toString + rep, rep) }
  }

  @Setup
  def setup(): Unit = {
    rep1Set = makeRep(rep1id)
    rep2Set = makeRep(rep2id)
    rep2Delta = rep2Set.addΔ("hallo welt", rep2id)
    rep1SetPlusOne = Lattice.merge(rep1Set, rep2Delta)
    justSetRep1 = rep1Set.store.keySet
    justSetRep2 = rep2Set.store.keySet
  }

  @Benchmark
  def create() = makeRep(rep1id)

  @Benchmark
  def addOne() = rep1Set.add("Hallo Welt", rep1id)

  @Benchmark
  def addOneDelta() = rep1Set.addΔ("Hallo Welt", rep1id)

  @Benchmark
  def containsNot() = rep1Set.contains("Hallo Welt")

  @Benchmark
  def containsFirst() = rep1Set.contains("0")

  @Benchmark
  def merge() = AddWinsSetO.latticeAddWinsSet.merge(rep1Set, rep2Set)

  @Benchmark
  def merge2() = AddWinsSetO.latticeAddWinsSetPerfOpt.merge(rep1Set, rep2Set)

  @Benchmark
  def mergeBaseline() = Lattice.merge(rep1Set.store, rep2Set.store)

  @Benchmark
  def mergeBaselineBaseline() = Lattice.merge(justSetRep1, justSetRep2)

  @Benchmark
  def mergeSelf() = Lattice.merge(rep1Set, rep1Set)

  @Benchmark
  def mergeSelfPlusOne() = Lattice.merge(rep1Set, rep1Set)

  @Benchmark
  def mergeDelta() = Lattice.merge(rep1Set, rep2Delta)

  @Benchmark
  def serializeUJson() = {
    import upickle.default._
    write(rep1Set)
  }

  @Benchmark
  def serializeJsoniter() = {
    com.github.plokhotnyuk.jsoniter_scala.core.writeToArray(rep1Set)
  }

  @Benchmark
  def serializeUJsonDelta() = {
    import upickle.default._
    write(rep2Delta)
  }

   @Benchmark
   def serializeJsoniterDelta() = {
     com.github.plokhotnyuk.jsoniter_scala.core.writeToArray(rep2Delta)
   }

}
