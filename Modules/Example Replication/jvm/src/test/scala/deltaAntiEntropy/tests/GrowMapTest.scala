package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import rdts.datatypes.GrowOnlyMap
import replication.JsoniterCodecs.given
import rdts.datatypes.GrowOnlyMap.{syntax, bottom}
import rdts.datatypes.contextual.ReplicatedSet
import org.scalacheck.Prop.forAll

import scala.collection.mutable

class GrowMapTest extends munit.ScalaCheckSuite {
  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  property("mutateKey/queryKey") {
    forAll { (add: List[Int], k: Int) =>
      val network = new Network(0, 0, 0)
      val aea     = new AntiEntropy[GrowOnlyMap[Int, ReplicatedSet[Int]]]("a", network, mutable.Buffer())
      val aeb     = new AntiEntropy[ReplicatedSet[Int]]("b", network, mutable.Buffer())

      val set = add.foldLeft(AntiEntropyContainer[ReplicatedSet[Int]](aeb)) {
        case (s, e) => s.add(using s.replicaID)(e)
      }

      val map: AntiEntropyContainer[GrowOnlyMap[Int, ReplicatedSet[Int]]] =
        add.foldLeft(AntiEntropyContainer[GrowOnlyMap[Int, ReplicatedSet[Int]]](aea)) {
          case (m, e) => m.mutateKeyNamedCtx(k, ReplicatedSet.empty[Int])((st) => st.add(using m.replicaID)(e))
        }

      val mapElements: Set[Int] = map.queryKey(k).map(o => o.elements).getOrElse(Set.empty[Int])

      assertEquals(
        mapElements,
        set.elements,
        s"Mutating/Querying a key in an ObserveRemoveMap should have the same behavior as modifying a standalone CRDT of that type, but $mapElements does not equal ${set.elements}"
      )
    }
  }
}
