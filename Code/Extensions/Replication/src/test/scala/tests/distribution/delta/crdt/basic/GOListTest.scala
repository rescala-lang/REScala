package tests.distribution.delta.crdt.basic

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.crdt.basic._
import rescala.extra.lattices.delta.Codecs._
import rescala.extra.lattices.delta.interfaces.GListInterface.GListAsUIJDLattice
import NetworkGenerators._

import scala.collection.mutable

object GListGenerators {
  def genGList[E: JsonValueCodec](implicit e: Arbitrary[E]): Gen[GList[E]] = for {
    elems <- Gen.containerOf[List, E](e.arbitrary)
  } yield {
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropy[GList.State[E]]("a", network, mutable.Buffer())

    elems.foldLeft(GList(ae)) {
      case (list, el) => list.insert(0, el)
    }
  }

  implicit def arbGList[E: JsonValueCodec](implicit e: Arbitrary[E]): Arbitrary[GList[E]] = Arbitrary(genGList)
}

class GOListTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import GListGenerators._

  implicit val IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  "size, toList, read" in forAll { (list: GList[Int], readIdx: Int) =>
    val l = list.toList

    assert(
      list.size == l.size,
      s"The size of a GList should equal the size of the list resulting from toList, but ${list.size} does not equal ${list.toList.size}"
    )
    assert(
      list.read(readIdx) == l.lift(readIdx),
      s"Reading from the GList at any valid index should return the same value as reading from the list returned by toList, but at index $readIdx ${list.read(readIdx)} does not equal ${l.lift(readIdx)}"
    )
  }

  "insert" in forAll { (list: GList[Int], n: Int, e: Int) =>
    val l = list.toList

    val inserted = list.insert(n, e)

    assert(
      n > list.size || n < 0 || inserted.size == list.size + 1,
      s"When an element is inserted into the list its size should increase by 1, but ${inserted.size} does not equal ${list.size} + 1"
    )
    assert(
      (n <= list.size || n >= 0) || inserted.toList == l,
      s"When insert is called with an invalid position the list should not change, but ${inserted.toList} does not equal ${list.toList}"
    )
    assert(
      n > list.size || n < 0 || inserted.read(n).contains(e),
      s"Reading the position where an element was inserted should return that element, but ${inserted.read(n)} does not contain $e"
    )

    val from = if (n < 0) 0 else n

    (from until l.size).foreach { i =>
      assert(
        n < 0 || inserted.read(i + 1).contains(l(i)),
        s"All elements after the insertion point should be shifted by 1, but element at ${i + 1} does not equal element originally at $i: ${inserted.read(i + 1)} does not contain ${l(i)}"
      )
    }
  }

  "concurrent insert" in forAll { (base: List[Int], n1: Int, e1: Int, n2: Int, e2: Int) =>
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropy[GList.State[Int]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[GList.State[Int]]("b", network, mutable.Buffer("a"))

    val la0 = base.reverse.foldLeft(GList(aea)) {
      case (l, e) => l.insert(0, e)
    }

    AntiEntropy.sync(aea, aeb)
    val lb0 = GList(aeb).processReceivedDeltas()

    val size = base.size
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

  "convergence" in forAll { (base: List[Int], insertedA: List[Int], insertedB: List[Int], network: Network) =>
    val aea = new AntiEntropy[GList.State[Int]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropy[GList.State[Int]]("b", network, mutable.Buffer("a"))

    val la0 = base.reverse.foldLeft(GList(aea)) {
      case (l, e) => l.insert(0, e)
    }
    network.startReliablePhase()
    AntiEntropy.sync(aea, aeb)
    network.endReliablePhase()
    val lb0 = GList(aeb).processReceivedDeltas()

    val la1 = insertedA.foldLeft(la0) { (l, e) => l.insert(e, e) }
    val lb1 = insertedB.foldLeft(lb0) { (l, e) => l.insert(e, e) }

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
