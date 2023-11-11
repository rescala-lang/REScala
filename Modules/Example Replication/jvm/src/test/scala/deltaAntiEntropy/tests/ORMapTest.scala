package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import kofre.datatypes.contextual.{AddWinsSet, ObserveRemoveMap}
import replication.JsoniterCodecs.*
import org.scalacheck.Prop.*

import scala.collection.mutable

class ORMapTest extends munit.ScalaCheckSuite {
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  property("mutateKey/queryKey") {
    forAll { (add: List[Int], remove: List[Int], k: Int) =>

      val network = new Network(0, 0, 0)
      val aea     = new AntiEntropy[ObserveRemoveMap[Int, AddWinsSet[Int]]]("a", network, mutable.Buffer())
      val aeb     = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer())

      val set = {
        val added: AntiEntropyContainer[AddWinsSet[Int]] = add.foldLeft(AntiEntropyContainer(aeb)) {
          case (s, e) => s.add(using s.replicaID)(e)
        }

        remove.foldLeft(added) {
          case (s, e) => s.remove(e)
        }
      }

      val map = {
        val added = add.foldLeft(AntiEntropyContainer[ObserveRemoveMap[Int, AddWinsSet[Int]]](aea)) {
          case (m, e) =>
            m.transform(k)(_.add(using m.replicaID)(e))
        }

        remove.foldLeft(added) {
          case (m, e) => m.transform(k)(_.remove(e))
        }
      }

      val mapElements = map.queryKey(k).elements

      assert(
        mapElements == set.elements,
        s"Mutating/Querying a key in an ObserveRemoveMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.elements}"
      )
    }
  }

  property("remove") {
    forAll { (add: List[Int], remove: List[Int], k: Int) =>
      val network = new Network(0, 0, 0)
      val aea =
        new AntiEntropy[ObserveRemoveMap[Int, AddWinsSet[Int]]]("a", network, mutable.Buffer())
      val aeb = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer())

      val empty = AntiEntropyContainer[AddWinsSet[Int]](aeb)

      val map = {
        val added = add.foldLeft(AntiEntropyContainer[ObserveRemoveMap[Int, AddWinsSet[Int]]](aea)) {
          case (m, e) => m.transform(k)(_.add(using m.replicaID)(e))
        }

        remove.foldLeft(added) {
          case (m, e) => m.transform(k)(_.remove(e))
        }
      }

      val removed = map.observeRemoveMap.remove(k)

      val queryResult = removed.queryKey(k).elements

      assertEquals(
        queryResult,
        empty.elements,
        s"Querying a removed key should produce the same result as querying an empty CRDT, but $queryResult does not equal ${empty.elements}"
      )
    }
  }

}
