package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Named, Network}
import kofre.base.Uid
import kofre.datatypes.contextual.ReplicatedList
import kofre.dotted.Dotted
import kofre.syntax.ReplicaId
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import replication.JsoniterCodecs.*

import test.kofre.DataGenerator.RGAGen.{makeRGA, given}

import scala.collection.mutable

object RGAGenerators {


  def makeNet[E: JsonValueCodec](rl: Dotted[ReplicatedList[E]]) =
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[ReplicatedList[E]]("a", network, mutable.Buffer())
    val aec     = AntiEntropyContainer[ReplicatedList[E]](ae)
    aec.applyDelta(Named(Uid.predefined("a"), rl))
    aec
}

class RGATest extends munit.ScalaCheckSuite {
  import RGAGenerators.*

  implicit val IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make


  property("size, toList, read") {
    forAll { (rl: Dotted[ReplicatedList[Int]], readIdx: Int) =>
      val listInitial: List[Int] = rl.toList
      val rga                    = makeNet(rl)

      val rgaList = rga.toList

      assertEquals(listInitial, rgaList)

      assertEquals(rga.size, listInitial.size, s"  ${rga.state}\n  $listInitial")

      assertEquals(rga.read(readIdx), listInitial.lift(readIdx))
    }

  }
  property("insert") {
    forAll { (rl: Dotted[ReplicatedList[Int]], insertIdx: Int, insertValue: Int) =>
      val rga      = makeNet(rl)
      val inserted = rga.insert(using rga.replicaID)(insertIdx, insertValue)

      assert(
        insertIdx < 0 || insertIdx > rga.size || inserted.read(insertIdx).contains(insertValue),
        s"After inserting a value at a valid index, reading the rga at that index should return the inserted value but ${inserted.read(
            insertIdx
          )} does not contain $insertValue\n  $rga\n  $inserted"
      )
      assert(
        (insertIdx >= 0 && insertIdx <= rga.size) || inserted.toList == rga.toList,
        s"Attempting to insertGL a value at an invalid index should not change the rga, but ${inserted.toList} does not equal ${rga.toList}"
      )
    }

  }
  property("delete") {
    forAll { (rl: Dotted[ReplicatedList[Int]], deleteIdx: Int) =>
      val rga        = makeNet(rl)
      val sizebefore = rga.size
      val listbefore = rga.toList
      val deleted    = rga.delete(using rga.replicaID)(deleteIdx)

      assert(
        deleteIdx < 0 || deleteIdx >= sizebefore || deleted.size == sizebefore - 1,
        s"After deleting a valid index the size of the rga should be reduced by 1, but ${deleted.size} does not equal ${rga.size} - 1"
      )
      assert(
        (deleteIdx >= 0 && deleteIdx < sizebefore) || deleted.toList == listbefore,
        s"Attempting to delete an invalid index should not change the rga, but ${deleted.toList} does not equal ${rga.toList}"
      )
    }

  }
  property("update") {
    // Potentially many wasted executions ...
    forAll { (rl: Dotted[ReplicatedList[Int]], updateIdx: Int, updateValue: Int) =>
      val rllist      = rl.toList
      val rga         = makeNet(rl)
      val initiallist = rga.toList
      val updated     = rga.update(using rga.replicaID)(updateIdx, updateValue)

      assert(
        updated.size == rga.size,
        s"update should not change the size of the rga, but ${updated.size} does not equal ${rga.size}"
      )
      assertEquals(rga.size, rga.toList.size)
      assert(
        updateIdx < 0 || updateIdx >= rga.size || updated.read(updateIdx).contains(updateValue),
        s"After updating a valid index reading the rga at that index should return the updated value, but ${updated.read(
            updateIdx
          )} does not contain $updateValue ($updateIdx) \n  ${rga.toList}\n  $rllist\n  ${initiallist}\n  ${updated.state}\n  $rl"
      )
      assert(
        (updateIdx >= 0 && updateIdx < rga.size) || updated.toList == rga.toList,
        s"Attempting to update an invalid index should not change th rga, but ${updated.toList} does not equal ${rga.toList}"
      )
    }

  }
  property("concurrent insert") {
    forAll {
      (inserted: List[(Int, Int)], removed: List[Int], n1: Int, e1: Int, n2: Int, e2: Int) =>
        val network = new Network(0, 0, 0)

        val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
        val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

        val la0 = AntiEntropyContainer(aea)
        la0.applyDelta(Named(Uid.predefined(aea.replicaID), makeRGA(inserted, removed, Uid.predefined(aea.replicaID))))
        AntiEntropy.sync(aea, aeb)
        val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

        val size = la0.size
        val idx1 = if (size == 0) 0 else math.floorMod(n1, size)
        val idx2 = if (size == 0) 0 else Math.floorMod(n2, size)

        val la1 = la0.insert(using la0.replicaID)(idx1, e1)
        lb0.insert(using lb0.replicaID)(idx2, e2)

        AntiEntropy.sync(aea, aeb)

        val la2 = la1.processReceivedDeltas()

        assert(
          idx1 < idx2 && la2.read(idx1).contains(e1) ||
          idx1 > idx2 && la2.read(idx1 + 1).contains(e1) ||
          idx1 == idx2 && (la2.read(idx1).contains(e1) || la2.read(idx1 + 1).contains(e1)),
          s"After synchronization $e1 was not found at its expected location in ${la2.toList}\n  ${la1} \n  ${lb0}"
        )
        assert(
          idx1 < idx2 && la2.read(idx2 + 1).contains(e2) ||
          idx1 > idx2 && la2.read(idx2).contains(e2) ||
          idx1 == idx2 && (la2.read(idx2).contains(e2) || la2.read(idx2 + 1).contains(e2)),
          s"After synchronization $e2 was not found at its expected location in ${la2.toList}"
        )
    }

  }
  property("concurrent delete") {
    forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, n1: Int, n2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

      val la0 = AntiEntropyContainer(aea)
      la0.applyDelta(Named(Uid.predefined(aea.replicaID), makeRGA(inserted, removed, Uid.predefined(aea.replicaID))))
      AntiEntropy.sync(aea, aeb)
      val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

      val idx = if (la0.size == 0) 0 else math.floorMod(n, la0.size)

      val la1 = la0.delete(using la0.replicaID)(idx)
      val lb1 = lb0.delete(using lb0.replicaID)(idx)

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()
      val lb2 = lb1.processReceivedDeltas()

      assert(
        la2.toList == la1.toList,
        s"Concurrently deleting the same index twice should have the same result as deleting it once, but ${la2.toList} does not equal ${la1.toList}"
      )

      val size = la2.size
      val idx1 = if (size == 0) 0 else math.floorMod(n1, size)
      val idx2 = if (size == 0) 0 else math.floorMod(n2, size)

      val la3 = la2.delete(using la2.replicaID)(idx1)
      lb2.delete(using lb2.replicaID)(idx2)

      AntiEntropy.sync(aea, aeb)

      val la4 = la3.processReceivedDeltas()

      val sequential =
        if (idx1 > idx2) {
          la2.delete(using la2.replicaID)(idx1).delete(using la2.replicaID)(idx2)
        } else {
          la2.delete(using la2.replicaID)(idx2).delete(using la2.replicaID)(idx1)
        }

      assert(
        idx1 == idx2 || la4.toList == sequential.toList,
        s"Concurrently deleting two different indices should have the same effect as sequential deletes, but ${la4.toList} does not equal ${sequential.toList}"
      )
    }

    // concurrent updates are resolved by timestamp and thus hard to test properly

  }
  property("concurrent insert update") {
    forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

      val la0 = AntiEntropyContainer(aea)
      la0.applyDelta(Named(Uid.predefined(aea.replicaID), makeRGA(inserted, removed, Uid.predefined(aea.replicaID))))
      AntiEntropy.sync(aea, aeb)
      val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

      val idx = if (la0.size == 0) 0 else math.floorMod(n, la0.size)

      val la1 = la0.insert(using la0.replicaID)(idx, e1)
      lb0.update(using lb0.replicaID)(idx, e2)

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()

      val sequential = la0.update(using la0.replicaID)(idx, e2).insert(using la0.replicaID)(idx, e1)

      assert(
        la2.toList == sequential.toList,
        s"Concurrent insertGL and update at the same index should have the same effect as sequential update then insertGL, but ${la2.toList} does not equal ${sequential.toList}"
      )
    }

  }
  property("concurrent insert delete") {
    forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, e: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

      val la0 = AntiEntropyContainer(aea)
      la0.applyDelta(Named(Uid.predefined(aea.replicaID), makeRGA(inserted, removed, Uid.predefined(aea.replicaID))))
      AntiEntropy.sync(aea, aeb)
      val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

      val idx = if (la0.size == 0) 0 else math.floorMod(n, la0.size)

      val la1 = la0.insert(using la0.replicaID)(idx + 1, e)
      lb0.delete(using lb0.replicaID)(idx)

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()

      val sequential = la0.insert(using la0.replicaID)(idx + 1, e).delete(using la0.replicaID)(idx)

      assert(
        la2.toList == sequential.toList,
        s"Inserting an element next to an element that was concurrently deleted should have the same index as sequential insertGL then delete, but ${la2.toList} does not equal ${sequential.toList}"
      )
    }

  }
  property("concurrent update delete") {
    forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, e: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

      val la0 = AntiEntropyContainer(aea)
      la0.applyDelta(Named(Uid.predefined(aea.replicaID), makeRGA(inserted, removed, Uid.predefined(aea.replicaID))))
      AntiEntropy.sync(aea, aeb)
      val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

      val idx = if (la0.size == 0) 0 else math.floorMod(n, la0.size)

      val la1 = la0.delete(using la0.replicaID)(idx)
      lb0.update(using lb0.replicaID)(idx, e)

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()

      assert(
        la2.toList == la1.toList,
        s"Update should have no effect if the updated index was concurrently deleted, but ${la2.toList} does not equal ${la1.toList}"
      )
    }

  }
  property("convergence") {
    forAll {
      (
          inserted: List[(Int, Int)],
          removed: List[Int],
          insertedAB: (List[(Int, Int)], List[(Int, Int)]),
          removedAB: (List[Int], List[Int]),
          updatedAB: (List[(Int, Int)], List[(Int, Int)]),
          network: Network
      ) =>
        {
          val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
          val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

          val la0 = AntiEntropyContainer(aea)
          la0.applyDelta(Named(
            Uid.predefined(aea.replicaID),
            makeRGA(inserted, removed, Uid.predefined(aea.replicaID))
          ))
          network.startReliablePhase()
          AntiEntropy.sync(aea, aeb)
          network.endReliablePhase()
          val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

          val la1 = {
            val inserted = insertedAB._1.foldLeft(la0) {
              case (rga, (i, e)) => rga.insert(using rga.replicaID)(i, e)
            }

            val deleted = removedAB._1.foldLeft(inserted) {
              case (rga, i) => rga.delete(using rga.replicaID)(i)
            }

            updatedAB._1.foldLeft(deleted) {
              case (rga, (i, e)) => rga.update(using rga.replicaID)(i, e)
            }
          }

          val lb1 = {
            val inserted = insertedAB._2.foldLeft(lb0) {
              case (rga, (i, e)) => rga.insert(using rga.replicaID)(i, e)
            }

            val deleted = removedAB._2.foldLeft(inserted) {
              case (rga, i) => rga.delete(using rga.replicaID)(i)
            }

            updatedAB._2.foldLeft(deleted) {
              case (rga, (i, e)) => rga.update(using rga.replicaID)(i, e)
            }
          }

          AntiEntropy.sync(aea, aeb)
          network.startReliablePhase()
          AntiEntropy.sync(aea, aeb)

          val la2 = la1.processReceivedDeltas()
          val lb2 = lb1.processReceivedDeltas()

          assert(
            la2.toList == lb2.toList,
            s"After synchronization messages were reliably exchanged all replicas should converge, but ${la2.toList} does not equal ${lb2.toList}"
          )
        }
    }
  }
}
