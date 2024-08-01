package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Named, Network}
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.*
import rdts.base.{Bottom, Lattice, LocalUid, Uid}
import rdts.datatypes.contextual.ReplicatedList
import rdts.dotted.Dotted
import replication.JsoniterCodecs.given
import test.rdts.DataGenerator.RGAGen.{makeRGA, given}

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

  given IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  property("size, toList, read") {
    forAll { (rl: Dotted[ReplicatedList[Int]], readIdx: Int) =>
      val listInitial: List[Int] = rl.data.toList
      val rga                    = makeNet(rl)

      val rgaList = rga.data.toList

      assertEquals(listInitial, rgaList)

      assertEquals(rga.data.size, listInitial.size, s"  ${rga.state}\n  $listInitial")

      assertEquals(rga.data.read(readIdx), listInitial.lift(readIdx))
    }

  }
  property("insert") {
    forAll { (rl: Dotted[ReplicatedList[Int]], insertIdx: Int, insertValue: Int) =>
      val rga      = makeNet(rl)
      val inserted = rga.mod(_.insert(using rga.replicaID)(insertIdx, insertValue))

      assert(
        insertIdx < 0 || insertIdx > rga.data.size || inserted.data.read(insertIdx).contains(insertValue),
        s"After inserting a value at a valid index, reading the rga at that index should return the inserted value but ${inserted.data.read(
            insertIdx
          )} does not contain $insertValue\n  $rga\n  $inserted"
      )
      assert(
        (insertIdx >= 0 && insertIdx <= rga.data.size) || inserted.data.toList == rga.data.toList,
        s"Attempting to insertGL a value at an invalid index should not change the rga, but ${inserted.data.toList} does not equal ${rga.data.toList}"
      )
    }

  }
  property("delete") {
    forAll { (rl: Dotted[ReplicatedList[Int]], deleteIdx: Int) =>
      val rga        = makeNet(rl)
      val sizebefore = rga.data.size
      val listbefore = rga.data.toList
      val deleted    = rga.mod(_.delete(using rga.replicaID)(deleteIdx))

      assert(
        deleteIdx < 0 || deleteIdx >= sizebefore || deleted.data.size == sizebefore - 1,
        s"After deleting a valid index the size of the rga should be reduced by 1, but ${deleted.data.size} does not equal ${rga.data.size} - 1"
      )
      assert(
        (deleteIdx >= 0 && deleteIdx < sizebefore) || deleted.data.toList == listbefore,
        s"Attempting to delete an invalid index should not change the rga, but ${deleted.data.toList} does not equal ${rga.data.toList}"
      )
    }

  }
  property("update") {
    // Potentially many wasted executions ...
    forAll { (rl: Dotted[ReplicatedList[Int]], updateIdx: Int, updateValue: Int) =>
      val rllist      = rl.data.toList
      val rga         = makeNet(rl)
      val initiallist = rga.data.toList
      val updated     = rga.mod(_.update(using rga.replicaID)(updateIdx, updateValue))

      assert(
        updated.data.size == rga.data.size,
        s"update should not change the size of the rga, but ${updated.data.size} does not equal ${rga.data.size}"
      )
      assertEquals(rga.data.size, rga.data.toList.size)
      assert(
        updateIdx < 0 || updateIdx >= rga.data.size || updated.data.read(updateIdx).contains(updateValue),
        s"After updating a valid index reading the rga at that index should return the updated value, but ${updated.data.read(
            updateIdx
          )} does not contain $updateValue ($updateIdx) \n  ${rga.data.toList}\n  $rllist\n  ${initiallist}\n  ${updated.state}\n  $rl"
      )
      assert(
        (updateIdx >= 0 && updateIdx < rga.data.size) || updated.data.toList == rga.data.toList,
        s"Attempting to update an invalid index should not change th rga, but ${updated.data.toList} does not equal ${rga.data.toList}"
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
        la0.applyDelta(Named(aea.localUid.uid, makeRGA(inserted, removed, aea.localUid)))
        AntiEntropy.sync(aea, aeb)
        val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

        val size = la0.data.size
        val idx1 = if size == 0 then 0 else math.floorMod(n1, size)
        val idx2 = if size == 0 then 0 else Math.floorMod(n2, size)

        val la1 = la0.mod(_.insert(using la0.replicaID)(idx1, e1))
        lb0.mod(_.insert(using lb0.replicaID)(idx2, e2))

        AntiEntropy.sync(aea, aeb)

        val la2 = la1.processReceivedDeltas()

        assert(
          idx1 < idx2 && la2.data.read(idx1).contains(e1) ||
          idx1 > idx2 && la2.data.read(idx1 + 1).contains(e1) ||
          idx1 == idx2 && (la2.data.read(idx1).contains(e1) || la2.data.read(idx1 + 1).contains(e1)),
          s"After synchronization $e1 was not found at its expected location in ${la2.data.toList}\n  ${la1} \n  ${lb0}"
        )
        assert(
          idx1 < idx2 && la2.data.read(idx2 + 1).contains(e2) ||
          idx1 > idx2 && la2.data.read(idx2).contains(e2) ||
          idx1 == idx2 && (la2.data.read(idx2).contains(e2) || la2.data.read(idx2 + 1).contains(e2)),
          s"After synchronization $e2 was not found at its expected location in ${la2.data.toList}"
        )
    }

  }
  property("concurrent delete") {
    forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, n1: Int, n2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

      val la0 = AntiEntropyContainer(aea)
      la0.applyDelta(Named(aea.localUid.uid, makeRGA(inserted, removed, aea.localUid)))
      AntiEntropy.sync(aea, aeb)
      val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

      val idx = if la0.data.size == 0 then 0 else math.floorMod(n, la0.data.size)

      val la1 = la0.mod(_.delete(using la0.replicaID)(idx))
      val lb1 = lb0.mod(_.delete(using lb0.replicaID)(idx))

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()
      val lb2 = lb1.processReceivedDeltas()

      assert(
        la2.data.toList == la1.data.toList,
        s"Concurrently deleting the same index twice should have the same result as deleting it once, but ${la2.data.toList} does not equal ${la1.data.toList}"
      )

      val size = la2.data.size
      val idx1 = if size == 0 then 0 else math.floorMod(n1, size)
      val idx2 = if size == 0 then 0 else math.floorMod(n2, size)

      val la3 = la2.mod(_.delete(using la2.replicaID)(idx1))
      lb2.mod(_.delete(using lb2.replicaID)(idx2))

      AntiEntropy.sync(aea, aeb)

      val la4 = la3.processReceivedDeltas()

      val sequential =
        if idx1 > idx2 then {
          la2.mod(_.delete(using la2.replicaID)(idx1)).mod(_.delete(using la2.replicaID)(idx2))
        } else {
          la2.mod(_.delete(using la2.replicaID)(idx2)).mod(_.delete(using la2.replicaID)(idx1))
        }

      assert(
        idx1 == idx2 || la4.data.toList == sequential.data.toList,
        s"Concurrently deleting two different indices should have the same effect as sequential deletes, but ${la4.data.toList} does not equal ${sequential.data.toList}"
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
      la0.applyDelta(Named(aea.localUid.uid, makeRGA(inserted, removed, aea.localUid)))
      AntiEntropy.sync(aea, aeb)
      val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

      val idx = if la0.data.size == 0 then 0 else math.floorMod(n, la0.data.size)

      val la1 = la0.mod(_.insert(using la0.replicaID)(idx, e1))
      lb0.mod(_.update(using lb0.replicaID)(idx, e2))

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()

      val sequential = la0.mod(_.update(using la0.replicaID)(idx, e2)).mod(_.insert(using la0.replicaID)(idx, e1))

      assert(
        la2.data.toList == sequential.data.toList,
        s"Concurrent insertGL and update at the same index should have the same effect as sequential update then insertGL, but ${la2.data.toList} does not equal ${sequential.data.toList}"
      )
    }

  }
  property("concurrent insert delete") {
    forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, e: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

      val la0 = AntiEntropyContainer(aea)
      la0.applyDelta(Named(aea.localUid.uid, makeRGA(inserted, removed, aea.localUid)))
      AntiEntropy.sync(aea, aeb)
      val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

      val idx = if la0.data.size == 0 then 0 else math.floorMod(n, la0.data.size)

      val la1 = la0.mod(_.insert(using la0.replicaID)(idx + 1, e))
      lb0.mod(_.delete(using lb0.replicaID)(idx))

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()

      val sequential = la0.mod(_.insert(using la0.replicaID)(idx + 1, e)).mod(_.delete(using la0.replicaID)(idx))

      assert(
        la2.data.toList == sequential.data.toList,
        s"Inserting an element next to an element that was concurrently deleted should have the same index as sequential insertGL then delete, but ${la2.data.toList} does not equal ${sequential.data.toList}"
      )
    }

  }
  property("concurrent update delete") {
    forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, e: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

      val la0 = AntiEntropyContainer(aea)
      la0.applyDelta(Named(aea.localUid.uid, makeRGA(inserted, removed, aea.localUid)))
      AntiEntropy.sync(aea, aeb)
      val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

      val idx = if la0.data.size == 0 then 0 else math.floorMod(n, la0.data.size)

      val la1 = la0.mod(_.delete(using la0.replicaID)(idx))
      lb0.mod(_.update(using lb0.replicaID)(idx, e))

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()

      assert(
        la2.data.toList == la1.data.toList,
        s"Update should have no effect if the updated index was concurrently deleted, but ${la2.data.toList} does not equal ${la1.data.toList}"
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
          networkGen: NetworkGenerator
      ) =>
        {
          val network = networkGen.make()

          val aea = new AntiEntropy[ReplicatedList[Int]]("a", network, mutable.Buffer("b"))
          val aeb = new AntiEntropy[ReplicatedList[Int]]("b", network, mutable.Buffer("a"))

          val la0 = AntiEntropyContainer(aea)
          la0.applyDelta(Named(aea.localUid.uid, makeRGA(inserted, removed, aea.localUid)))
          network.startReliablePhase()
          AntiEntropy.sync(aea, aeb)
          network.endReliablePhase()
          val lb0 = AntiEntropyContainer[ReplicatedList[Int]](aeb).processReceivedDeltas()

          val la1 = {
            val inserted = insertedAB._1.foldLeft(la0) {
              case (rga, (i, e)) => rga.mod(_.insert(using rga.replicaID)(i, e))
            }

            val deleted = removedAB._1.foldLeft(inserted) {
              case (rga, i) => rga.mod(_.delete(using rga.replicaID)(i))
            }

            updatedAB._1.foldLeft(deleted) {
              case (rga, (i, e)) => rga.mod(_.update(using rga.replicaID)(i, e))
            }
          }

          val lb1 = {
            val inserted = insertedAB._2.foldLeft(lb0) {
              case (rga, (i, e)) => rga.mod(_.insert(using rga.replicaID)(i, e))
            }

            val deleted = removedAB._2.foldLeft(inserted) {
              case (rga, i) => rga.mod(_.delete(using rga.replicaID)(i))
            }

            updatedAB._2.foldLeft(deleted) {
              case (rga, (i, e)) => rga.mod(_.update(using rga.replicaID)(i, e))
            }
          }

          val beforeA1: Dotted[ReplicatedList[Int]] = la1.state
          val beforeB1                              = lb1.state

          AntiEntropy.sync(aea, aeb)
          network.startReliablePhase()
          AntiEntropy.sync(aea, aeb)

          val la2 = la1.processReceivedDeltas()
          val lb2 = lb1.processReceivedDeltas()

          assertEquals(
            beforeA1.decomposed.reduceOption(Lattice.merge).getOrElse(Bottom.empty[Dotted[ReplicatedList[Int]]]),
            beforeA1
          )
          assertEquals(
            beforeB1.decomposed.reduceOption(Lattice.merge).getOrElse(Bottom.empty[Dotted[ReplicatedList[Int]]]),
            beforeB1
          )

          val directMergedState = beforeA1 `merge` beforeB1
          assertEquals(lb2.state, directMergedState)
          assertEquals(la2.state, directMergedState)

          assertEquals(
            la2.data.toList,
            lb2.data.toList,
            s"After synchronization messages were reliably exchanged all replicas should converge, but ${la2.data.toList} does not equal ${lb2.data.toList}\n  before A: $beforeA1\n  before B: $beforeB1\n  ${la2.state}\n  ${lb2.state}"
          )
        }
    }
  }
}
