package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import kofre.base.{Lattice, Uid}
import kofre.datatypes.GrowOnlyList
import kofre.datatypes.contextual.ReplicatedList
import kofre.dotted.{Dotted, HasDots}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import replication.JsoniterCodecs.*

import scala.collection.mutable

class AntiEntropyBasicTest extends munit.ScalaCheckSuite {

  implicit val IntCodec: JsonValueCodec[String] = JsonCodecMaker.make
  given HasDots[Int]                            = HasDots.noDots
  given HasDots[String]                         = HasDots.noDots

  test("basic") {

    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[ReplicatedList[String]]("a", network, mutable.Buffer())

    val aec = AntiEntropyContainer[ReplicatedList[String]](ae)

    aec.insert(using aec.replicaID)(0, "00")

    assertEquals(aec.toList, List("00"))

    aec.insert(using aec.replicaID)(1, "100")

    assertEquals(aec.toList, List("00", "100"))

    val lots = List.tabulate(100)(_.toString)

    lots.foreach: elem =>
      aec.insert(using aec.replicaID)(0, elem)
    // aec.insertAll(using aec.replicaID)(0, lots)

    assertEquals(aec.toList, lots.reverse ::: List("00", "100"))

    aec.insert(using Uid.predefined("b"))(1, "b00")

    assertEquals(aec.read(1), Some("b00"))

  }

  test("basic grow only list") {

    val network = new Network(0, 0, 0)

    val ae = new AntiEntropy[GrowOnlyList[String]]("a", network, mutable.Buffer())

    val aec = AntiEntropyContainer(ae)

    aec.insertGL(0, "00")

    assertEquals(aec.toList, List("00"))

    aec.insertGL(1, "100")

    assertEquals(aec.toList, List("00", "100"), aec.state)

    val lots = List.tabulate(100)(_.toString)

    lots.foreach: elem =>
      aec.insertGL(0, elem)
    // aec.insertAll(using aec.replicaID)(0, lots)

    assertEquals(aec.toList, lots.reverse ::: List("00", "100"))

    aec.insertGL(1, "b00")

    assertEquals(aec.read(1), Some("b00"))

  }

}
