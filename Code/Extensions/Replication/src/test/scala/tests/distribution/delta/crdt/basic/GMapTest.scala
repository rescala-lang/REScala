package tests.distribution.delta.crdt.basic

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.decompose.interfaces.AWSetInterface.{AWSet, AWSetSyntax}
import kofre.decompose.interfaces.GMapInterface.GMap
import kofre.syntax.AllPermissionsCtx
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.replication.AntiEntropy
import kofre.decompose.interfaces.GMapInterface.GMapSyntax
import kofre.decompose.containers.{AntiEntropyCRDT, Network}

import scala.collection.mutable

class GMapTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  "mutateKey/queryKey" in { (add: List[Int], k: Int) =>
    val network = new Network(0, 0, 0)
    val aea     = new AntiEntropy[GMap[Int, AWSet[Int]]]("a", network, mutable.Buffer())
    val aeb     = new AntiEntropy[AWSet[Int]]("b", network, mutable.Buffer())

    val set = add.foldLeft(AntiEntropyCRDT[AWSet[Int]](aeb)) {
      case (s, e) => s.add(e)
    }

    val map = add.foldLeft(AntiEntropyCRDT[GMap[Int, AWSet[Int]]](aea)) {
      case (m, e) => m.mutateKey(k)((st) => st.add(e)(AllPermissionsCtx.withID(m.replicaID)))
    }

    val mapElements = map.queryKey(k).elements

    assert(
      mapElements == set.elements,
      s"Mutating/Querying a key in an ORMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.elements}"
    )
  }
}
