package tests.distribution.delta.crdt

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.CContext._
import rescala.extra.lattices.delta.{AntiEntropy, CContext, Network}
import rescala.extra.lattices.delta.crdt.RGA
import rescala.extra.lattices.delta.crdt.RGA._
import tests.distribution.delta.NetworkGenerators.arbNetwork

import scala.collection.mutable

object RGAGenerators {
  def makeRGA[E: JsonValueCodec, C: CContext](
      inserted: List[(Int, E)],
      removed: List[Int],
      ae: AntiEntropy[RGA.State[E, C]]
  ): RGA[E, C] = {
    val afterInsert = inserted.foldLeft(RGA(ae)) {
      case (rga, (i, e)) => rga.insert(i, e)
    }

    removed.foldLeft(afterInsert) {
      case (rga, i) => rga.delete(i)
    }
  }

  def genRGA[E: JsonValueCodec, C: CContext](implicit e: Arbitrary[E], codec: JsonValueCodec[C]): Gen[RGA[E, C]] = for {
    nInserted       <- Arbitrary.arbitrary[Byte]
    insertedIndices <- Gen.containerOfN[List, Int](nInserted.toInt, Arbitrary.arbitrary[Int])
    insertedValues  <- Gen.containerOfN[List, E](nInserted.toInt, e.arbitrary)
    removed         <- Gen.containerOf[List, Int](Arbitrary.arbitrary[Int])
  } yield {
    val network = new Network(0, 0, 0)

    val ae = new AntiEntropy[RGA.State[E, C]]("a", network, mutable.Buffer())

    makeRGA(insertedIndices zip insertedValues, removed, ae)
  }

  implicit def arbRGA[E: JsonValueCodec, C: CContext](implicit
      e: Arbitrary[E],
      codec: JsonValueCodec[C]
  ): Arbitrary[RGA[E, C]] =
    Arbitrary(genRGA)
}

class RGATest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import RGAGenerators._

  implicit val IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  "size, toList, read" in forAll { (rga: RGA[Int, DietMapCContext], readIdx: Int) =>
    val listInitial = rga.toList

    assert(
      rga.size == listInitial.size,
      s"The size of the rga should equal the size of the list returned by toList, but ${rga.size} does not equal ${listInitial.size}"
    )

    assert(
      rga.read(readIdx) == listInitial.lift(readIdx),
      s"Reading the rga at an index should produce the same result as accessing the list returned by toList, but at index $readIdx ${rga.read(readIdx)} does not equal ${listInitial.lift(readIdx)}"
    )
  }

  "insert" in forAll { (rga: RGA[Int, DietMapCContext], insertIdx: Int, insertValue: Int) =>
    val inserted = rga.insert(insertIdx, insertValue)

    assert(
      insertIdx < 0 || insertIdx > rga.size || inserted.read(insertIdx).contains(insertValue),
      s"After inserting a value at a valid index, reading the rga at that index should return the inserted value but ${inserted.read(insertIdx)} does not contain $insertValue"
    )
    assert(
      (insertIdx >= 0 && insertIdx <= rga.size) || inserted.toList == rga.toList,
      s"Attempting to insert a value at an invalid index should not change the rga, but ${inserted.toList} does not equal ${rga.toList}"
    )
  }

  "delete" in forAll { (rga: RGA[Int, DietMapCContext], deleteIdx: Int) =>
    val deleted = rga.delete(deleteIdx)

    assert(
      deleteIdx < 0 || deleteIdx >= rga.size || deleted.size == rga.size - 1,
      s"After deleting a valid index the size of the rga should be reduced by 1, but ${deleted.size} does not equal ${rga.size} - 1"
    )
    assert(
      (deleteIdx >= 0 && deleteIdx < rga.size) || deleted.toList == rga.toList,
      s"Attempting to delete an invalid index should not change the rga, but ${deleted.toList} does not equal ${rga.toList}"
    )
  }

  "update" in forAll { (rga: RGA[Int, DietMapCContext], updateIdx: Int, updateValue: Int) =>
    val updated = rga.update(updateIdx, updateValue)

    assert(
      updated.size == rga.size,
      s"update should not change the size of the rga, but ${updated.size} does not equal ${rga.size}"
    )
    assert(
      updateIdx < 0 || updateIdx >= rga.size || updated.read(updateIdx).contains(updateValue),
      s"After updating a valid index reading the rga at that index should return the updated value, but ${updated.read(updateIdx)} does not contain $updateValue"
    )
    assert(
      (updateIdx >= 0 && updateIdx < rga.size) || updated.toList == rga.toList,
      s"Attempting to update an invalid index should not change th rga, but ${updated.toList} does not equal ${rga.toList}"
    )
  }

  "concurrent insert" in forAll {
    (inserted: List[(Int, Int)], removed: List[Int], n1: Int, e1: Int, n2: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[RGA.State[Int, DietMapCContext]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[RGA.State[Int, DietMapCContext]]("b", network, mutable.Buffer("a"))

      val la0 = makeRGA(inserted, removed, aea)
      AntiEntropy.sync(aea, aeb)
      val lb0 = RGA(aeb).processReceivedDeltas()

      val size = la0.size
      val idx1 = if (size == 0) 0 else math.floorMod(n1, size)
      val idx2 = if (size == 0) 0 else Math.floorMod(n2, size)

      val la1 = la0.insert(idx1, e1)
      lb0.insert(idx2, e2)

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()

      assert(
        idx1 < idx2 && la2.read(idx1).contains(e1) ||
          idx1 > idx2 && la2.read(idx1 + 1).contains(e1) ||
          idx1 == idx2 && (la2.read(idx1).contains(e1) || la2.read(idx1 + 1).contains(e1)),
        s"After synchronization $e1 was not found at its expected location in ${la2.toList}"
      )
      assert(
        idx1 < idx2 && la2.read(idx2 + 1).contains(e2) ||
          idx1 > idx2 && la2.read(idx2).contains(e2) ||
          idx1 == idx2 && (la2.read(idx2).contains(e2) || la2.read(idx2 + 1).contains(e2)),
        s"After synchronization $e2 was not found at its expected location in ${la2.toList}"
      )
  }

  "concurrent delete" in forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, n1: Int, n2: Int) =>
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[RGA.State[Int, DietMapCContext]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[RGA.State[Int, DietMapCContext]]("b", network, mutable.Buffer("a"))

    val la0 = makeRGA(inserted, removed, aea)
    AntiEntropy.sync(aea, aeb)
    val lb0 = RGA(aeb).processReceivedDeltas()

    val idx = if (la0.size == 0) 0 else math.floorMod(n, la0.size)

    val la1 = la0.delete(idx)
    val lb1 = lb0.delete(idx)

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

    val la3 = la2.delete(idx1)
    lb2.delete(idx2)

    AntiEntropy.sync(aea, aeb)

    val la4 = la3.processReceivedDeltas()

    val sequential =
      if (idx1 > idx2) {
        la2.delete(idx1).delete(idx2)
      } else {
        la2.delete(idx2).delete(idx1)
      }

    assert(
      idx1 == idx2 || la4.toList == sequential.toList,
      s"Concurrently deleting two different indices should have the same effect as sequential deletes, but ${la4.toList} does not equal ${sequential.toList}"
    )
  }

  // concurrent updates are resolved by timestamp and thus hard to test properly

  "concurrent insert update" in forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, e1: Int, e2: Int) =>
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[RGA.State[Int, DietMapCContext]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[RGA.State[Int, DietMapCContext]]("b", network, mutable.Buffer("a"))

    val la0 = makeRGA(inserted, removed, aea)
    AntiEntropy.sync(aea, aeb)
    val lb0 = RGA(aeb).processReceivedDeltas()

    val idx = if (la0.size == 0) 0 else math.floorMod(n, la0.size)

    val la1 = la0.insert(idx, e1)
    lb0.update(idx, e2)

    AntiEntropy.sync(aea, aeb)

    val la2 = la1.processReceivedDeltas()

    val sequential = la0.update(idx, e2).insert(idx, e1)

    assert(
      la2.toList == sequential.toList,
      s"Concurrent insert and update at the same index should have the same effect as sequential update then insert, but ${la2.toList} does not equal ${sequential.toList}"
    )
  }

  "concurrent insert delete" in forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, e: Int) =>
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[RGA.State[Int, DietMapCContext]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[RGA.State[Int, DietMapCContext]]("b", network, mutable.Buffer("a"))

    val la0 = makeRGA(inserted, removed, aea)
    AntiEntropy.sync(aea, aeb)
    val lb0 = RGA(aeb).processReceivedDeltas()

    val idx = if (la0.size == 0) 0 else math.floorMod(n, la0.size)

    val la1 = la0.insert(idx + 1, e)
    lb0.delete(idx)

    AntiEntropy.sync(aea, aeb)

    val la2 = la1.processReceivedDeltas()

    val sequential = la0.insert(idx + 1, e).delete(idx)

    assert(
      la2.toList == sequential.toList,
      s"Inserting an element next to an element that was concurrently deleted should have the same index as sequential insert then delete, but ${la2.toList} does not equal ${sequential.toList}"
    )
  }

  "concurrent update delete" in forAll { (inserted: List[(Int, Int)], removed: List[Int], n: Int, e: Int) =>
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[RGA.State[Int, DietMapCContext]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[RGA.State[Int, DietMapCContext]]("b", network, mutable.Buffer("a"))

    val la0 = makeRGA(inserted, removed, aea)
    AntiEntropy.sync(aea, aeb)
    val lb0 = RGA(aeb).processReceivedDeltas()

    val idx = if (la0.size == 0) 0 else math.floorMod(n, la0.size)

    val la1 = la0.delete(idx)
    lb0.update(idx, e)

    AntiEntropy.sync(aea, aeb)

    val la2 = la1.processReceivedDeltas()

    assert(
      la2.toList == la1.toList,
      s"Update should have no effect if the updated index was concurrently deleted, but ${la2.toList} does not equal ${la1.toList}"
    )
  }

  "convergence" in forAll {
    (
        inserted: List[(Int, Int)],
        removed: List[Int],
        insertedAB: (List[(Int, Int)], List[(Int, Int)]),
        removedAB: (List[Int], List[Int]),
        updatedAB: (List[(Int, Int)], List[(Int, Int)]),
        network: Network
    ) =>
      {
        val aea = new AntiEntropy[RGA.State[Int, DietMapCContext]]("a", network, mutable.Buffer("b"))
        val aeb = new AntiEntropy[RGA.State[Int, DietMapCContext]]("b", network, mutable.Buffer("a"))

        val la0 = makeRGA(inserted, removed, aea)
        network.startReliablePhase()
        AntiEntropy.sync(aea, aeb)
        network.endReliablePhase()
        val lb0 = RGA(aeb).processReceivedDeltas()

        val la1 = {
          val inserted = insertedAB._1.foldLeft(la0) {
            case (rga, (i, e)) => rga.insert(i, e)
          }

          val deleted = removedAB._1.foldLeft(inserted) {
            case (rga, i) => rga.delete(i)
          }

          updatedAB._1.foldLeft(deleted) {
            case (rga, (i, e)) => rga.update(i, e)
          }
        }

        val lb1 = {
          val inserted = insertedAB._2.foldLeft(lb0) {
            case (rga, (i, e)) => rga.insert(i, e)
          }

          val deleted = removedAB._2.foldLeft(inserted) {
            case (rga, i) => rga.delete(i)
          }

          updatedAB._2.foldLeft(deleted) {
            case (rga, (i, e)) => rga.update(i, e)
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
