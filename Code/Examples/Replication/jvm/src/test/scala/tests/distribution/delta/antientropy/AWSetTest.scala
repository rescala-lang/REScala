package tests.distribution.delta.antientropy

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalacheck.{Arbitrary, Gen}
import replication.JsoniterCodecs._
import NetworkGenerators._
import kofre.decompose.containers.Network
import kofre.datatypes.AddWinsSet
import org.scalacheck.Prop._
import testtools.{AntiEntropy, AntiEntropyCRDT}

import scala.collection.mutable

object AWSetGenerators {
  def genAWSet[A: JsonValueCodec](implicit a: Arbitrary[A]): Gen[AntiEntropyCRDT[AddWinsSet[A]]] =
    for {
      added   <- Gen.containerOf[List, A](a.arbitrary)
      n       <- Gen.choose(0, added.size)
      removed <- Gen.pick(n, added)
    } yield {
      val network = new Network(0, 0, 0)
      val ae      = new AntiEntropy[AddWinsSet[A]]("a", network, mutable.Buffer())

      val setAdded = added.foldLeft(AntiEntropyCRDT[AddWinsSet[A]](ae)) {
        case (set, e) => set.add(e)
      }
      removed.foldLeft(setAdded) {
        case (set, e) => set.remove(e)
      }
    }

  implicit def arbAWSet[A: JsonValueCodec](implicit
      a: Arbitrary[A]
  ): Arbitrary[AntiEntropyCRDT[AddWinsSet[A]]] =
    Arbitrary(genAWSet[A])
}

class AWSetTest extends munit.ScalaCheckSuite {
  import AWSetGenerators._

  implicit val IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make
  property("add") {
    forAll { (set: AntiEntropyCRDT[AddWinsSet[Int]], e: Int) =>
      val added: AntiEntropyCRDT[AddWinsSet[Int]] = set.add(e)

      val elems = added.elements
      assert(
        elems.contains(e),
        s"After adding an element to the set it should be contained in its elements, but ${elems} does not contain $e"
      )
    }
  }
  property("remove") {
    forAll { (set: AntiEntropyCRDT[AddWinsSet[Int]], e: Int) =>
      val removedNotContained = set.remove(e)
      val added               = set.add(e)
      val removed             = added.remove(e)

      assert(
        set.elements.contains(e) || removedNotContained.elements == set.elements,
        s"Removing an element that was not contained in the set should not change the set, but ${removedNotContained.elements} does not equal ${set.elements}"
      )

      assert(
        !removed.elements.contains(e),
        s"When removing an element from a set the resulting set should not contain this element, but ${removed.elements} contains $e"
      )
    }
  }
  property("clear") {
    forAll { (set: AntiEntropyCRDT[AddWinsSet[Int]]) =>
      val cleared = set.clear()

      assert(
        cleared.elements.isEmpty,
        s"After clearing the set it should be empty, but ${cleared.elements} is not empty"
      )
    }
  }
  property("concurrent add") {
    forAll { (e: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[AddWinsSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer("a"))

      val seta0 = AntiEntropyCRDT[AddWinsSet[Int]](aea)
      val setb0 = AntiEntropyCRDT[AddWinsSet[Int]](aeb)

      val seta1 = seta0.add(e)
      val setb1 = setb0.add(e)

      AntiEntropy.sync(aea, aeb)

      val seta2 = seta1.processReceivedDeltas()
      val setb2 = setb1.processReceivedDeltas()

      assert(
        seta2.elements.contains(e),
        s"Concurrently adding the same element should have the same effect as adding it once, but ${seta2.elements} does not contain $e"
      )
      assert(
        setb2.elements.contains(e),
        s"Concurrently adding the same element should have the same effect as adding it once, but ${setb2.elements} does not contain $e"
      )

      val seta3 = seta2.add(e1)
      val setb3 = setb2.add(e2)

      AntiEntropy.sync(aea, aeb)

      val seta4 = seta3.processReceivedDeltas()
      val setb4 = setb3.processReceivedDeltas()

      assert(
        Set(e1, e2).subsetOf(seta4.elements),
        s"Concurrently adding two different elements should result in a set containing both elements, but ${seta4.elements} does not contain both $e1 and $e2"
      )
      assert(
        Set(e1, e2).subsetOf(setb4.elements),
        s"Concurrently adding two different elements should result in a set containing both elements, but ${setb4.elements} does not contain both $e1 and $e2"
      )
    }
  }
  property("concurrent remove") {
    forAll { (e: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[AddWinsSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer("a"))

      val seta0 = AntiEntropyCRDT[AddWinsSet[Int]](aea).add(e).add(e1).add(e2)
      aea.sendChangesToAllNeighbors()
      aeb.receiveFromNetwork()
      val setb0 = AntiEntropyCRDT[AddWinsSet[Int]](aeb).processReceivedDeltas()

      val seta1 = seta0.remove(e)
      val setb1 = setb0.remove(e)

      AntiEntropy.sync(aea, aeb)

      val seta2 = seta1.processReceivedDeltas()
      val setb2 = setb1.processReceivedDeltas()

      assert(
        !seta2.elements.contains(e),
        s"Concurrently removing the same element should have the same effect as removing it once, but ${seta2.elements} contains $e"
      )
      assert(
        !setb2.elements.contains(e),
        s"Concurrently removing the same element should have the same effect as removing it once, but ${setb2.elements} contains $e"
      )

      val seta3 = seta2.remove(e1)
      val setb3 = setb2.remove(e2)

      AntiEntropy.sync(aea, aeb)

      val seta4 = seta3.processReceivedDeltas()
      val setb4 = setb3.processReceivedDeltas()

      assert(
        Set(e1, e2).intersect(seta4.elements).isEmpty,
        s"Concurrently removing two different elements should result in a set not containing either element, but ${seta4.elements} contains one of $e1, $e2"
      )
      assert(
        Set(e1, e2).intersect(setb4.elements).isEmpty,
        s"Concurrently removing two different elements should result in a set not containing either element, but ${setb4.elements} contains one of $e1, $e2"
      )
    }
  }
  property("concurrent add/remove") {
    forAll { (e: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[AddWinsSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer("a"))

      val seta0 = AntiEntropyCRDT[AddWinsSet[Int]](aea).add(e2)
      aea.sendChangesToAllNeighbors()
      aeb.receiveFromNetwork()
      val setb0 = AntiEntropyCRDT[AddWinsSet[Int]](aeb).processReceivedDeltas()

      val seta1 = seta0.add(e)
      val setb1 = setb0.remove(e)

      AntiEntropy.sync(aea, aeb)

      val seta2 = seta1.processReceivedDeltas()
      val setb2 = setb1.processReceivedDeltas()

      assert(
        seta2.elements.contains(e),
        s"When concurrently adding and removing the same element the add operation should win, but ${seta2.elements} does not contain $e"
      )
      assert(
        setb2.elements.contains(e),
        s"When concurrently adding and removing the same element the add operation should win, but ${setb2.elements} does not contain $e"
      )

      val seta3 = seta2.add(e1)
      val setb3 = setb2.remove(e2)

      AntiEntropy.sync(aea, aeb)

      val seta4 = seta3.processReceivedDeltas()
      val setb4 = setb3.processReceivedDeltas()

      assert(
        e1 == e2 || (seta4.elements.contains(e1) && !seta4.elements.contains(e2)),
        s"Concurrently adding an element and removing another should have the same effects as sequential execution, but ${seta4.elements} either contains $e2 or does not contain $e1"
      )
      assert(
        e1 == e2 || (setb4.elements.contains(e1) && !setb4.elements.contains(e2)),
        s"Concurrently adding an element and removing another should have the same effects as sequential execution, but ${setb4.elements} either contains $e2 or does not contain $e1"
      )
    }
  }
  property("concurrent add/clear") {
    forAll { (e: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[AddWinsSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer("a"))

      val seta0 = AntiEntropyCRDT[AddWinsSet[Int]](aea).add(e1).add(e2)
      AntiEntropy.sync(aea, aeb)
      val setb0 = AntiEntropyCRDT[AddWinsSet[Int]](aeb).processReceivedDeltas()

      val seta1 = seta0.add(e)
      val setb1 = setb0.clear()

      AntiEntropy.sync(aea, aeb)

      val seta2 = seta1.processReceivedDeltas()
      val setb2 = setb1.processReceivedDeltas()

      assert(
        seta2.elements == Set(e),
        s"Concurrently adding an element and clearing the set should result in a singleton set containing the added element, but ${seta2.elements} does not equal ${Set(e)}"
      )
      assert(
        setb2.elements == Set(e),
        s"Concurrently adding an element and clearing the set should result in a singleton set containing the added element, but ${setb2.elements} does not equal ${Set(e)}"
      )
    }
  }
  property("convergence") {
    forAll {
      (addedA: List[Int], removedA: List[Int], addedB: List[Int], removedB: List[Int], network: Network) =>
        val aea = new AntiEntropy[AddWinsSet[Int]]("a", network, mutable.Buffer("b"))
        val aeb = new AntiEntropy[AddWinsSet[Int]]("b", network, mutable.Buffer("a"))

        val setaAdded = addedA.foldLeft(AntiEntropyCRDT[AddWinsSet[Int]](aea)) {
          case (set, e) => set.add(e)
        }
        val seta0 = removedA.foldLeft(setaAdded) {
          case (set, e) => set.remove(e)
        }

        val setbAdded = addedB.foldLeft(AntiEntropyCRDT[AddWinsSet[Int]](aeb)) {
          case (set, e) => set.add(e)
        }
        val setb0 = removedB.foldLeft(setbAdded) {
          case (set, e) => set.remove(e)
        }

        AntiEntropy.sync(aea, aeb)
        network.startReliablePhase()
        AntiEntropy.sync(aea, aeb)

        val seta1 = seta0.processReceivedDeltas()
        val setb1 = setb0.processReceivedDeltas()

        assert(
          seta1.elements == setb1.elements,
          s"After synchronization messages were reliably exchanged all replicas should converge, but ${seta1.elements} does not equal ${setb1.elements}"
        )
    }
  }
}
