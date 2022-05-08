package tests.distribution.delta.antientropy

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.JsoniterCodecs._
import rescala.extra.replication.AntiEntropy
import kofre.decompose.interfaces.ORMapInterface.ORMap
import kofre.syntax.AllPermissionsCtx
import kofre.decompose.interfaces.ORMapInterface.ORMapSyntax
import kofre.decompose.containers.{AntiEntropyCRDT, Network}
import kofre.predef.AddWinsSet

import scala.collection.mutable

class ORMapTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  "mutateKey/queryKey" in { (add: List[Int], remove: List[Int], k: Int) =>
    val network = new Network(0, 0, 0)
    val aea =
      new AntiEntropy[ORMap[Int, AddWinsSet.Embedded[Int]]]("a", network, mutable.Buffer())
    val aeb = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer())

    val set = {
      val added = add.foldLeft(AntiEntropyCRDT(aeb)) {
        case (s, e) => s.add(e)
      }

      remove.foldLeft(added) {
        case (s, e) => s.remove(e)
      }
    }

    val map = {
      val added = add.foldLeft(AntiEntropyCRDT[ORMap[Int, AddWinsSet.Embedded[Int]]](aea)) {
        case (m, e) =>
          m.mutateKey(k, (id, st) => AddWinsSet(st).add(e)(AllPermissionsCtx.withID(id)).inner)
      }

      remove.foldLeft(added) {
        case (m, e) => m.mutateKey(k, (id, st) => AddWinsSet(st).remove(e)(AllPermissionsCtx.withID(id)).inner)
      }
    }

    val mapElements = AddWinsSet(map.queryKey(k)).elements

    assert(
      mapElements == set.elements,
      s"Mutating/Querying a key in an ORMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.elements}"
    )
  }

  "remove" in { (add: List[Int], remove: List[Int], k: Int) =>
    val network = new Network(0, 0, 0)
    val aea =
      new AntiEntropy[ORMap[Int, AddWinsSet.Embedded[Int]]]("a", network, mutable.Buffer())
    val aeb = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer())

    val empty = AntiEntropyCRDT[AddWinsSet[Int]](aeb)

    val map = {
      val added = add.foldLeft(AntiEntropyCRDT[ORMap[Int, AddWinsSet.Embedded[Int]]](aea)) {
        case (m, e) => m.mutateKey(k, (id, st) => AddWinsSet(st).add(e)(AllPermissionsCtx.withID(id)).inner)
      }

      remove.foldLeft(added) {
        case (m, e) => m.mutateKey(k, (id, st) => st.remove(e)(AllPermissionsCtx.withID(id), implicitly))
      }
    }

    val removed = map.remove(k)

    val queryResult = AddWinsSet(removed.queryKey(k)).elements

    assert(
      queryResult == empty.elements,
      s"Querying a removed key should produce the same result as querying an empty CRDT, but $queryResult does not equal ${empty.elements}"
    )
  }
}
