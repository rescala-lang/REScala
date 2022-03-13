package tests.distribution.delta.crdt.basic

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.decompose.interfaces.AWSetInterface.{AWSet, AWSetSyntax}
import kofre.syntax.AllPermissionsCtx
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.lattices.delta.crdt.basic.{AntiEntropyCRDT, GMap, Network}
import rescala.extra.replication.AntiEntropy

import scala.collection.mutable

class GMapTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  "mutateKey/queryKey" in { (add: List[Int], k: Int) =>
    val network = new Network(0, 0, 0)
    val aea     = new AntiEntropy[GMap.State[Int, AWSet[Int]]]("a", network, mutable.Buffer())
    val aeb     = new AntiEntropy[AWSet[Int]]("b", network, mutable.Buffer())

    val set = add.foldLeft(AntiEntropyCRDT[AWSet[Int]](aeb)) {
      case (s, e) => s.add(e)
    }

    val map = add.foldLeft(GMap[Int, AWSet[Int]](aea)) {
      case (m, e) => m.mutateKey(k, (id, st) => st.add(e)(AllPermissionsCtx.withID(id)))
    }

    val mapElements = map.queryKey(k, (st) => st.elements)

    assert(
      mapElements == set.elements,
      s"Mutating/Querying a key in an ORMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.elements}"
    )
  }
}
