package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Named, Network}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.datatypes.GrowOnlyList
import rdts.dotted.{Dotted, HasDots}
import replication.JsoniterCodecs.given

import scala.collection.mutable

object GListGenerators {
  def genGList[E](using e: Arbitrary[E]): Gen[GrowOnlyList[E]] =
    for
      elems <- Gen.listOfN(20, e.arbitrary)
    yield {
      elems.foldLeft(GrowOnlyList.empty[E]) {
        case (list, el) => list `merge` list.insertGL(0, el)
      }
    }

  given arbGList[E: {JsonValueCodec, HasDots}](using
      e: Arbitrary[E]
  ): Arbitrary[GrowOnlyList[E]] =
    Arbitrary(genGList)

  def makeNet[E: {JsonValueCodec, HasDots}](v: GrowOnlyList[E]) =
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[GrowOnlyList[E]]("a", network, mutable.Buffer())
    val aec     = AntiEntropyContainer[GrowOnlyList[E]](ae)
    aec.applyDelta(Named(aec.replicaID.uid, Dotted(v)))

}

class GListTest extends munit.ScalaCheckSuite {
  import GListGenerators.{*, given}

  given IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make
  given HasDots[Int]                  = HasDots.noDots

  property("size, toList, read") {
    forAll { (gol: GrowOnlyList[Int], readIdx: Int) =>
      val list = makeNet(gol)
      val l    = list.data.toList

      assertEquals(list.data.size, l.size)
      assert(list.data.read(readIdx) == l.lift(readIdx))
    }
  }
  property("insert") {
    forAll { (gol: GrowOnlyList[Int], insertIndex: Int, e: Int) =>
      val list = makeNet(gol)

      val szeBefore = list.data.size
      val l         = list.data.toList

      l.zipWithIndex.foreach: (e, i) =>
        assertEquals(list.data.read(i), Some(e))

      assertEquals(szeBefore, l.size)

      val n = if szeBefore == 0 then 0 else (insertIndex % szeBefore).abs

      list.modn(_.insertGL(n, e))

      val inserted =
        val (b, a) = l.splitAt(n)
        b ::: (e :: a)

      assertEquals(list.data.read(n), Some(e), s"$n ${insertIndex},\n  ${l}\n  ${list.data.toList}\n  ${list.state}")

      assert(
        list.data.size == szeBefore + 1,
        s"When an element is inserted into the list its size should increase by 1, but ${list.data.size} does not equal ${szeBefore} + 1"
      )

      assertEquals(list.data.toList.toSet, inserted.toSet)
      assertEquals(list.data.toList.sorted, inserted.sorted)
      assertEquals(list.data.toList, inserted)
    }
  }
  property("toLazyList") {
    forAll { (gol: GrowOnlyList[Int]) =>
      val list  = makeNet(gol)
      val l     = list.data.toList
      val lazyl = list.data.toLazyList.toList

      assert(
        lazyl == l,
        s"Converting a glist to a lazylist and then a list should have the same result as turning it into a list directly, but $lazyl does not equal $l"
      )
    }
  }
  property("concurrent insert") {
    forAll { (base: List[Int], n1: Int, e1: Int, n2: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[GrowOnlyList[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[GrowOnlyList[Int]]("b", network, mutable.Buffer("a"))

      val la0 = base.reverse.foldLeft(AntiEntropyContainer[GrowOnlyList[Int]](aea)) {
        case (l, e) => l.modn(_.insertGL(0, e))
      }

      AntiEntropy.sync(aea, aeb)
      val lb0 = AntiEntropyContainer[GrowOnlyList[Int]](aeb).processReceivedDeltas()

      val size = base.size
      val idx1 = if size == 0 then 0 else math.floorMod(n1, size)
      val idx2 = if size == 0 then 0 else Math.floorMod(n2, size)

      val la1 = la0.modn(_.insertGL(idx1, e1))
      lb0.modn(_.insertGL(idx2, e2))

      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()

      assert(
        idx1 < idx2 && la2.data.read(idx1).contains(e1) ||
        idx1 > idx2 && la2.data.read(idx1 + 1).contains(e1) ||
        idx1 == idx2 && (la2.data.read(idx1).contains(e1) || la2.data.read(idx1 + 1).contains(e1)),
        s"After synchronization $e1 was not found at its expected location in ${la2.data.toList}"
      )
      assert(
        idx1 < idx2 && la2.data.read(idx2 + 1).contains(e2) ||
        idx1 > idx2 && la2.data.read(idx2).contains(e2) ||
        idx1 == idx2 && (la2.data.read(idx2).contains(e2) || la2.data.read(idx2 + 1).contains(e2)),
        s"After synchronization $e2 was not found at its expected location in ${la2.data.toList}"
      )
    }
  }
  property("convergence") {
    forAll { (base: List[Int], insertedA: List[Int], insertedB: List[Int], networkGen: NetworkGenerator) =>
      val network = networkGen.make()
      val aea     = new AntiEntropy[GrowOnlyList[Int]]("a", network, mutable.Buffer("b"))
      val aeb     = new AntiEntropy[GrowOnlyList[Int]]("b", network, mutable.Buffer("a"))

      val la0 = base.reverse.foldLeft(AntiEntropyContainer[GrowOnlyList[Int]](aea)) {
        case (l, e) => l.modn(_.insertGL(0, e))
      }
      network.startReliablePhase()
      AntiEntropy.sync(aea, aeb)
      network.endReliablePhase()
      val lb0 = AntiEntropyContainer[GrowOnlyList[Int]](aeb).processReceivedDeltas()

      val la1 = insertedA.foldLeft(la0) { (l, e) => l.modn(_.insertGL(e, e)) }
      val lb1 = insertedB.foldLeft(lb0) { (l, e) => l.modn(_.insertGL(e, e)) }

      AntiEntropy.sync(aea, aeb)
      network.startReliablePhase()
      AntiEntropy.sync(aea, aeb)

      val la2 = la1.processReceivedDeltas()
      val lb2 = lb1.processReceivedDeltas()

      assert(
        la2.data.toList == lb2.data.toList,
        s"After synchronization messages were reliably exchanged all replicas should converge, but ${la2.data.toList} does not equal ${lb2.data.toList}"
      )
    }
  }
}
