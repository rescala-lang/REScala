package tests.distribution.delta.crdt.basic

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.DietCC.DietMapCContext
import rescala.extra.lattices.delta.crdt.basic.{AWSet, AntiEntropy, AntiEntropyImpl, GMap, Network}
import rescala.extra.lattices.delta.Codecs._
import rescala.extra.lattices.delta.interfaces.AWSetInterface

import scala.collection.mutable

class GMapTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  "mutateKey/queryKey" in { (add: List[Int], k: Int) =>
    val network = new Network(0, 0, 0)
    val aea     = new AntiEntropyImpl[GMap.State[Int, AWSet.State[Int, DietMapCContext]]]("a", network, mutable.Buffer())
    val aeb     = new AntiEntropyImpl[AWSet.State[Int, DietMapCContext]]("b", network, mutable.Buffer())

    val set = add.foldLeft(AWSet[Int, DietMapCContext](aeb)) {
      case (s, e) => s.add(e)
    }

    val map = add.foldLeft(GMap[Int, AWSet.State[Int, DietMapCContext]](aea)) {
      case (m, e) => m.mutateKey(k, AWSetInterface.add(e))
    }

    val mapElements = map.queryKey(k, AWSetInterface.elements)

    assert(
      mapElements == set.elements,
      s"Mutating/Querying a key in an ORMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.elements}"
    )
  }
}
