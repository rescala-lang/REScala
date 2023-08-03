package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import kofre.base.Lattice
import kofre.datatypes.GrowOnlyList
import kofre.datatypes.contextual.ReplicatedList
import kofre.dotted.{Dotted, HasDots}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import replication.JsoniterCodecs.*

import scala.collection.mutable

class AntiEntropyBasicTest extends munit.ScalaCheckSuite {

  implicit val IntCodec: JsonValueCodec[String] = JsonCodecMaker.make
  given HasDots[Int]                         = HasDots.noDots

  test("basic") {

    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[ReplicatedList[String]]("a", network, mutable.Buffer())

    val aec = AntiEntropyContainer[ReplicatedList[String]](ae)

    aec.insert(using aec.replicaID)(0, "00")

    assertEquals(aec.toList, List("00"))

  }

}
