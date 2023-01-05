package tests.distribution.delta.antientropy

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import replication.JsoniterCodecs._
import kofre.datatypes.ObserveRemoveMap.contextDecompose
import kofre.decompose.containers.Network
import kofre.datatypes.{AddWinsSet, ObserveRemoveMap}
import testtools.{AntiEntropy, AntiEntropyCRDT}

import scala.collection.mutable

class ORMapTest extends munit.ScalaCheckSuite {
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  test("mutateKey/queryKey") { (add: List[Int], remove: List[Int], k: Int) =>
    val network = new Network(0, 0, 0)
    val aea     = new AntiEntropy[ObserveRemoveMap[Int, AddWinsSet[Int]]]("a", network, mutable.Buffer())
    val aeb     = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer())

    val set = {
      val added: AntiEntropyCRDT[AddWinsSet[Int]] = add.foldLeft(AntiEntropyCRDT(aeb)) {
        case (s, e) => s.add(e)
      }

      remove.foldLeft(added) {
        case (s, e) => s.remove(e)
      }
    }

    val map = {
      val added = add.foldLeft(AntiEntropyCRDT[ObserveRemoveMap[Int, AddWinsSet[Int]]](aea)) {
        case (m, e) =>
          m.mutateKeyNamedCtx(k)(_.add(e))
      }

      remove.foldLeft(added) {
        case (m, e) => m.mutateKeyNamedCtx(k)(_.remove(e))
      }
    }

    val mapElements = map.queryKey(k).elements

    assert(
      mapElements == set.elements,
      s"Mutating/Querying a key in an ObserveRemoveMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.elements}"
    )
  }

  test("remove") { (add: List[Int], remove: List[Int], k: Int) =>
    val network = new Network(0, 0, 0)
    val aea =
      new AntiEntropy[ObserveRemoveMap[Int, AddWinsSet[Int]]]("a", network, mutable.Buffer())
    val aeb = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer())

    val empty = AntiEntropyCRDT[AddWinsSet[Int]](aeb)

    val map = {
      val added = add.foldLeft(AntiEntropyCRDT[ObserveRemoveMap[Int, AddWinsSet[Int]]](aea)) {
        case (m, e) => m.mutateKeyNamedCtx(k)(_.add(e))
      }

      remove.foldLeft(added) {
        case (m, e) => m.mutateKeyNamedCtx(k)(_.remove(e))
      }
    }

    val removed = map.remove(k)

    val queryResult = removed.queryKey(k).elements

    assert(
      queryResult == empty.elements,
      s"Querying a removed key should produce the same result as querying an empty CRDT, but $queryResult does not equal ${empty.elements}"
    )
  }
}
