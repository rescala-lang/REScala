package benchmarks.lattices

import java.util.concurrent.TimeUnit

import benchmarks.lattices.Codecs._
import org.openjdk.jmh.annotations._
import rescala.core.Struct
import rescala.extra.lattices.sets.AddWinsSetO
import rescala.extra.lattices.{IdUtil, Lattice}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class AddWinsSetOBench[S <: Struct] {

  @Param(Array("0", "1", "10", "100", "1000"))
  var setSize: Int = _

  var rep1Set       : AddWinsSetO[String] = _
  var rep1SetPlusOne: AddWinsSetO[String] = _
  var rep2Set       : AddWinsSetO[String] = _
  val rep1id                             = IdUtil.genId()
  val rep2id                             = IdUtil.genId()
  var rep2Δ         : AddWinsSetO[String] = _


  private def makeRep(rep: IdUtil.Id): AddWinsSetO[String] = {
    0.until(setSize).foldLeft(AddWinsSetO.empty[String]) { case (s, v) => s.add(v.toString + rep, rep) }
  }

  @Setup
  def setup(): Unit = {
    rep1Set = makeRep(rep1id)
    rep2Set = makeRep(rep2id)
    rep2Δ = rep2Set.addΔ("hallo welt", rep2id)
    rep1SetPlusOne = Lattice.merge(rep1Set, rep2Δ)
  }


  @Benchmark
  def create() = makeRep(rep1id)

  @Benchmark
  def addOne() = rep1Set.add("Hallo Welt", rep1id)

  @Benchmark
  def addOneΔ() = rep1Set.addΔ("Hallo Welt", rep1id)

  @Benchmark
  def containsNot() = rep1Set.contains("Hallo Welt")

  @Benchmark
  def containsFirst() = rep1Set.contains("0")

  @Benchmark
  def merge() = Lattice.merge(rep1Set, rep2Set)

  @Benchmark
  def mergeSelf() = Lattice.merge(rep1Set, rep1Set)

  @Benchmark
  def mergeSelfPlusOne() = Lattice.merge(rep1Set, rep1Set)


  @Benchmark
  def mergeΔ() = Lattice.merge(rep1Set, rep2Δ)


  @Benchmark
  def serializeUJson() = {
    import upickle.default._
    write(rep1Set)
  }

  @Benchmark
  def serializeCirce() = {
    import io.circe.syntax._
    rep1Set.asJson.noSpaces
  }

  @Benchmark
  def serializeUJsonΔ() = {
    import upickle.default._
    write(rep2Δ)
  }

  @Benchmark
  def serializeCirceΔ() = {
    import io.circe.syntax._
    rep2Δ.asJson.noSpaces
  }

}

