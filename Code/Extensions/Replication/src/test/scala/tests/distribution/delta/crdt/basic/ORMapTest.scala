package tests.distribution.delta.crdt.basic

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import kofre.decompose.interfaces.AWSetInterface
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.lattices.delta.crdt.basic._
import rescala.extra.replication.AntiEntropy

import scala.collection.mutable

class ORMapTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  "mutateKey/queryKey" in { (add: List[Int], remove: List[Int], k: Int) =>
    val network = new Network(0, 0, 0)
    val aea =
      new AntiEntropy[ORMap.State[Int, AWSet.Embedded[Int]]]("a", network, mutable.Buffer())
    val aeb = new AntiEntropy[AWSet.State[Int]]("b", network, mutable.Buffer())

    val set = {
      val added = add.foldLeft(AWSet[Int](aeb)) {
        case (s, e) => s.add(e)
      }

      remove.foldLeft(added) {
        case (s, e) => s.remove(e)
      }
    }

    val map = {
      val added = add.foldLeft(ORMap[Int, AWSet.Embedded[Int]](aea)) {
        case (m, e) => m.mutateKey(k, AWSetInterface.add(e))
      }

      remove.foldLeft(added) {
        case (m, e) => m.mutateKey(k, AWSetInterface.remove(e))
      }
    }

    val mapElements = map.queryKey(k, AWSetInterface.elements)

    assert(
      mapElements == set.elements,
      s"Mutating/Querying a key in an ORMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.elements}"
    )
  }

  "remove" in { (add: List[Int], remove: List[Int], k: Int) =>
    val network = new Network(0, 0, 0)
    val aea =
      new AntiEntropy[ORMap.State[Int, AWSet.Embedded[Int]]]("a", network, mutable.Buffer())
    val aeb = new AntiEntropy[AWSet.State[Int]]("b", network, mutable.Buffer())

    val empty = AWSet[Int](aeb)

    val map = {
      val added = add.foldLeft(ORMap[Int, AWSet.Embedded[Int]](aea)) {
        case (m, e) => m.mutateKey(k, AWSetInterface.add(e))
      }

      remove.foldLeft(added) {
        case (m, e) => m.mutateKey(k, AWSetInterface.remove(e))
      }
    }

    val removed = map.remove(k)

    val queryResult = removed.queryKey(k, AWSetInterface.elements)

    assert(
      queryResult == empty.elements,
      s"Querying a removed key should produce the same result as querying an empty CRDT, but $queryResult does not equal ${empty.elements}"
    )
  }
}
