package benchmarks.lattices

import java.util.concurrent.TimeUnit
import org.openjdk.jmh.annotations._
import kofre.dotbased.{AddWinsSet}
import kofre.causality.{ArrayRanges, CausalContext, Dot}
import kofre.{Defs, Lattice}

@BenchmarkMode(Array(Mode.Throughput))
@OutputTimeUnit(TimeUnit.MILLISECONDS)
@Warmup(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Measurement(iterations = 3, time = 1000, timeUnit = TimeUnit.MILLISECONDS)
@Fork(3)
@Threads(1)
@State(Scope.Thread)
class AddWinsSetBench {

  @Param(Array("10"))
  var setSize: Int = _

  var rep1Set: AddWinsSet[String]        = _
  var rep1SetPlusOne: AddWinsSet[String] = _
  var rep2Set: AddWinsSet[String]        = _
  val rep1id                             = Defs.genId()
  val rep2id                             = Defs.genId()
  var rep2Delta: AddWinsSet[String]      = _

  private def makeRep(rep: Defs.Id): AddWinsSet[String] = {
    0.until(setSize).foldLeft(AddWinsSet.empty[String]) { case (s, v) => s.add(v.toString + rep, rep) }
  }

  @Setup
  def setup(): Unit = {
    rep1Set = makeRep(rep1id)
    rep2Set = makeRep(rep2id)
    rep2Delta = rep2Set.addΔ("hallo welt", rep2id)
    rep1SetPlusOne = Lattice.merge(rep1Set, rep2Delta)
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
  def merge() = Lattice.merge(rep1Set, rep2Set)

  @Benchmark
  def mergeSelf() = Lattice.merge(rep1Set, rep1Set)

  @Benchmark
  def mergeSelfPlusOne() = Lattice.merge(rep1Set, rep1Set)

  @Benchmark
  def mergeDelta() = Lattice.merge(rep1Set, rep2Delta)

  @Benchmark
  def serializeUJson() = {
    import upickle.default._
    import Codecs.awsUJsonCodec
    write(rep1Set)
  }

  @Benchmark
  def serializeJsoniter() = {
    import Codecs.awsJsoiterCodec
    com.github.plokhotnyuk.jsoniter_scala.core.writeToArray(rep1Set)
  }

  @Benchmark
  def serializeUJsonDelta() = {
    import upickle.default._
    import Codecs.awsUJsonCodec
    write(rep2Delta)
  }

  @Benchmark
  def serializeJsoniterDelta() = {
    import Codecs.awsJsoiterCodec
    com.github.plokhotnyuk.jsoniter_scala.core.writeToArray(rep2Delta)
  }

}

object Codecs {

  implicit val dotUJsonCodec: upickle.default.ReadWriter[Dot]                   = upickle.default.macroRW

  implicit val arrayRangesCodec: upickle.default.ReadWriter[ArrayRanges] = upickle.default.macroRW
  implicit val contextCodec: upickle.default.ReadWriter[CausalContext]   = upickle.default.macroRW

  implicit val awsUJsonCodec: upickle.default.ReadWriter[AddWinsSet[String]]   = upickle.default.macroRW

  import com.github.plokhotnyuk.jsoniter_scala.core._
  import com.github.plokhotnyuk.jsoniter_scala.macros._

  implicit val awsJsoiterCodec: JsonValueCodec[AddWinsSet[String]] =
    JsonCodecMaker.make(CodecMakerConfig.withAllowRecursiveTypes(true))

}
