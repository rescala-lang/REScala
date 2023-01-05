package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyCRDT, Network}
import kofre.datatypes.{AddWinsSet, GrowOnlyMap}
import replication.JsoniterCodecs.*
import kofre.datatypes.GrowOnlyMap.given

import scala.collection.mutable

class GrowMapTest extends munit.ScalaCheckSuite {
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  test("mutateKey/queryKey") { (add: List[Int], k: Int) =>
    val network = new Network(0, 0, 0)
    val aea     = new AntiEntropy[GrowOnlyMap[Int, AddWinsSet[Int]]]("a", network, mutable.Buffer())
    val aeb     = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer())

    val set = add.foldLeft(AntiEntropyCRDT[AddWinsSet[Int]](aeb)) {
      case (s, e) => s.add(e)
    }

    val map = add.foldLeft(AntiEntropyCRDT[GrowOnlyMap[Int, AddWinsSet[Int]]](aea)) {
      case (m, e) => m.mutateKeyNamedCtx(k, AddWinsSet.empty[Int])((st) => st.add(e))
    }

    val mapElements = map.queryKey(k).map(_.elements).getOrElse(AddWinsSet.empty[Int])

    assert(
      mapElements == set.elements,
      s"Mutating/Querying a key in an ObserveRemoveMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.elements}"
    )
  }
}
