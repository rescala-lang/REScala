package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import org.scalacheck.Prop.*
import rdts.base
import rdts.base.{Bottom, LocalUid}
import rdts.datatypes.contextual.{ObserveRemoveMap, ReplicatedSet}
import rdts.dotted.Dotted
import replication.JsoniterCodecs.given

import scala.collection.mutable

class ORMapTest extends munit.ScalaCheckSuite {
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  property("contains") {
    given LocalUid = base.LocalUid.predefined("test")
    given Bottom[Int] with
      def empty = Int.MinValue
    forAll { (entries: List[Int]) =>
      val orMap = entries.foldLeft(Dotted(ObserveRemoveMap.empty[Int, Int])) { (curr, elem) =>
        curr.mod(_.update(elem, elem))
      }
      orMap.data.entries.foreach { (k, v) =>
        assert(orMap.data.contains(k))
      }
    }
  }

  property("mutateKey/queryKey") {
    forAll { (add: List[Int], remove: List[Int], k: Int) =>

      val network = new Network(0, 0, 0)
      val aea     = new AntiEntropy[ObserveRemoveMap[Int, ReplicatedSet[Int]]]("a", network, mutable.Buffer())
      val aeb     = new AntiEntropy[ReplicatedSet[Int]]("b", network, mutable.Buffer())

      val set = {
        val added: AntiEntropyContainer[ReplicatedSet[Int]] = add.foldLeft(AntiEntropyContainer(aeb)) {
          case (s, e) => s.mod(_.add(using s.replicaID)(e))
        }

        remove.foldLeft(added) {
          case (s, e) => s.mod(_.remove(e))
        }
      }

      val map = {
        val added = add.foldLeft(AntiEntropyContainer[ObserveRemoveMap[Int, ReplicatedSet[Int]]](aea)) {
          case (m, e) =>
            m.mod(_.transform(k)(_.mod(_.add(using m.replicaID)(e))))
        }

        remove.foldLeft(added) {
          case (m, e) => m.mod(_.transform(k)(_.mod(_.remove(e))))
        }
      }

      val mapElements = map.data.queryKey(k).elements

      assert(
        mapElements == set.data.elements,
        s"Mutating/Querying a key in an ObserveRemoveMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.data.elements}"
      )
    }
  }

  property("remove") {
    forAll { (add: List[Int], remove: List[Int], k: Int) =>
      val network = new Network(0, 0, 0)
      val aea =
        new AntiEntropy[ObserveRemoveMap[Int, ReplicatedSet[Int]]]("a", network, mutable.Buffer())
      val aeb = new AntiEntropy[ReplicatedSet[Int]]("b", network, mutable.Buffer())

      val empty = AntiEntropyContainer[ReplicatedSet[Int]](aeb)

      val map = {
        val added = add.foldLeft(AntiEntropyContainer[ObserveRemoveMap[Int, ReplicatedSet[Int]]](aea)) {
          case (m, e) => m.mod(_.transform(k)(_.mod(_.add(using m.replicaID)(e))))
        }

        remove.foldLeft(added) {
          case (m, e) => m.mod(_.transform(k)(_.mod(_.remove(e))))
        }
      }

      val removed = map.mod(_.remove(k))

      val queryResult = removed.data.queryKey(k).elements

      assertEquals(
        queryResult,
        empty.data.elements,
        s"Querying a removed key should produce the same result as querying an empty CRDT, but $queryResult does not equal ${empty.data.elements}"
      )
    }
  }

}
