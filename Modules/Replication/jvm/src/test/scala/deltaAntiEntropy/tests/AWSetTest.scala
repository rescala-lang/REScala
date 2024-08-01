package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.base.{LocalUid, Uid}
import rdts.datatypes.contextual.ReplicatedSet
import replication.JsoniterCodecs.given

import scala.collection.mutable

object AWSetGenerators {
  def genAWSet[A: JsonValueCodec](implicit a: Arbitrary[A]): Gen[AntiEntropyContainer[ReplicatedSet[A]]] =
    for
      added   <- Gen.containerOf[List, A](a.arbitrary)
      n       <- Gen.choose(0, added.size)
      removed <- Gen.pick(n, added)
    yield {
      val network = new Network(0, 0, 0)
      val ae      = new AntiEntropy[ReplicatedSet[A]]("a", network, mutable.Buffer())

      given LocalUid = LocalUid.predefined(ae.replicaID)

      val setAdded = added.foldLeft(AntiEntropyContainer[ReplicatedSet[A]](ae)) {
        case (set, e) => set.mod(_.add(e))
      }
      removed.foldLeft(setAdded) {
        case (set, e) => set.mod(_.remove(e))
      }
    }

  given arbAWSet[A: JsonValueCodec](using
      a: Arbitrary[A]
  ): Arbitrary[AntiEntropyContainer[ReplicatedSet[A]]] =
    Arbitrary(genAWSet[A])
}

class AWSetTest extends munit.ScalaCheckSuite {
  import AWSetGenerators.{*, given}

  given IntCodec: JsonValueCodec[Int] = JsonCodecMaker.make
  property("add") {
    forAll { (set: AntiEntropyContainer[ReplicatedSet[Int]], e: Int) =>
      given LocalUid                                      = set.replicaID
      val added: AntiEntropyContainer[ReplicatedSet[Int]] = set.mod(_.add(e))

      val elems = added.data.elements
      assert(
        elems.contains(e),
        s"After adding an element to the set it should be contained in its elements, but ${elems} does not contain $e"
      )
    }
  }
  property("remove") {
    forAll { (set: AntiEntropyContainer[ReplicatedSet[Int]], e: Int) =>
      given LocalUid          = set.replicaID
      val removedNotContained = set.mod(_.remove(e))
      val added               = set.mod(_.add(e))
      val removed             = added.mod(_.remove(e))

      assert(
        set.data.elements.contains(e) || removedNotContained.data.elements == set.data.elements,
        s"Removing an element that was not contained in the set should not change the set, but ${removedNotContained.data.elements} does not equal ${set.data.elements}"
      )

      assert(
        !removed.data.elements.contains(e),
        s"When removing an element from a set the resulting set should not contain this element, but ${removed.data.elements} contains $e"
      )
    }
  }
  property("clear") {
    forAll { (set: AntiEntropyContainer[ReplicatedSet[Int]]) =>
      val cleared = set.mod(_.clear())

      assert(
        cleared.data.elements.isEmpty,
        s"After clearing the set it should be empty, but ${cleared.data.elements} is not empty"
      )
    }
  }
  property("concurrent add") {
    forAll { (e: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedSet[Int]]("b", network, mutable.Buffer("a"))

      val seta0 = AntiEntropyContainer[ReplicatedSet[Int]](aea)
      val setb0 = AntiEntropyContainer[ReplicatedSet[Int]](aeb)

      val seta1 = seta0.mod(_.add(using seta0.replicaID)(e))
      val setb1 = setb0.mod(_.add(using setb0.replicaID)(e))

      AntiEntropy.sync(aea, aeb)

      val seta2 = seta1.processReceivedDeltas()
      val setb2 = setb1.processReceivedDeltas()

      assert(
        seta2.data.elements.contains(e),
        s"Concurrently adding the same element should have the same effect as adding it once, but ${seta2.data.elements} does not contain $e"
      )
      assert(
        setb2.data.elements.contains(e),
        s"Concurrently adding the same element should have the same effect as adding it once, but ${setb2.data.elements} does not contain $e"
      )

      val seta3 = seta2.mod(_.add(using seta2.replicaID)(e1))
      val setb3 = setb2.mod(_.add(using setb2.replicaID)(e2))

      AntiEntropy.sync(aea, aeb)

      val seta4 = seta3.processReceivedDeltas()
      val setb4 = setb3.processReceivedDeltas()

      assert(
        Set(e1, e2).subsetOf(seta4.data.elements),
        s"Concurrently adding two different elements should result in a set containing both elements, but ${seta4.data.elements} does not contain both $e1 and $e2"
      )
      assert(
        Set(e1, e2).subsetOf(setb4.data.elements),
        s"Concurrently adding two different elements should result in a set containing both elements, but ${setb4.data.elements} does not contain both $e1 and $e2"
      )
    }
  }
  property("concurrent remove") {
    forAll { (e: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedSet[Int]]("b", network, mutable.Buffer("a"))

      val seta0 =
        given LocalUid = LocalUid.predefined(aea.replicaID)
        AntiEntropyContainer[ReplicatedSet[Int]](aea).mod(_.add(e)).mod(_.add(e1)).mod(_.add(e2))
      aea.sendChangesToAllNeighbors()
      aeb.receiveFromNetwork()
      val setb0 = AntiEntropyContainer[ReplicatedSet[Int]](aeb).processReceivedDeltas()

      val seta1 = seta0.mod(_.remove(e))
      val setb1 = setb0.mod(_.remove(e))

      AntiEntropy.sync(aea, aeb)

      val seta2 = seta1.processReceivedDeltas()
      val setb2 = setb1.processReceivedDeltas()

      assert(
        !seta2.data.elements.contains(e),
        s"Concurrently removing the same element should have the same effect as removing it once, but ${seta2.data.elements} contains $e"
      )
      assert(
        !setb2.data.elements.contains(e),
        s"Concurrently removing the same element should have the same effect as removing it once, but ${setb2.data.elements} contains $e"
      )

      val seta3 = seta2.mod(_.remove(e1))
      val setb3 = setb2.mod(_.remove(e2))

      AntiEntropy.sync(aea, aeb)

      val seta4 = seta3.processReceivedDeltas()
      val setb4 = setb3.processReceivedDeltas()

      assert(
        Set(e1, e2).intersect(seta4.data.elements).isEmpty,
        s"Concurrently removing two different elements should result in a set not containing either element, but ${seta4.data.elements} contains one of $e1, $e2"
      )
      assert(
        Set(e1, e2).intersect(setb4.data.elements).isEmpty,
        s"Concurrently removing two different elements should result in a set not containing either element, but ${setb4.data.elements} contains one of $e1, $e2"
      )
    }
  }
  property("concurrent add/remove") {
    forAll { (e: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedSet[Int]]("b", network, mutable.Buffer("a"))

      val seta0 =
        given LocalUid = LocalUid.predefined(aea.replicaID)
        AntiEntropyContainer[ReplicatedSet[Int]](aea).mod(_.add(e2))
      aea.sendChangesToAllNeighbors()
      aeb.receiveFromNetwork()
      val setb0 = AntiEntropyContainer[ReplicatedSet[Int]](aeb).processReceivedDeltas()

      val seta1 = seta0.mod(_.add(using seta0.replicaID)(e))
      val setb1 = setb0.mod(_.remove(e))

      AntiEntropy.sync(aea, aeb)

      val seta2 = seta1.processReceivedDeltas()
      val setb2 = setb1.processReceivedDeltas()

      assert(
        seta2.data.elements.contains(e),
        s"When concurrently adding and removing the same element the add operation should win, but ${seta2.data.elements} does not contain $e"
      )
      assert(
        setb2.data.elements.contains(e),
        s"When concurrently adding and removing the same element the add operation should win, but ${setb2.data.elements} does not contain $e"
      )

      val seta3 = seta2.mod(_.add(using seta2.replicaID)(e1))
      val setb3 = setb2.mod(_.remove(e2))

      AntiEntropy.sync(aea, aeb)

      val seta4 = seta3.processReceivedDeltas()
      val setb4 = setb3.processReceivedDeltas()

      assert(
        e1 == e2 || (seta4.data.elements.contains(e1) && !seta4.data.elements.contains(e2)),
        s"Concurrently adding an element and removing another should have the same effects as sequential execution, but ${seta4.data.elements} either contains $e2 or does not contain $e1"
      )
      assert(
        e1 == e2 || (setb4.data.elements.contains(e1) && !setb4.data.elements.contains(e2)),
        s"Concurrently adding an element and removing another should have the same effects as sequential execution, but ${setb4.data.elements} either contains $e2 or does not contain $e1"
      )
    }
  }
  property("concurrent add/clear") {
    forAll { (e: Int, e1: Int, e2: Int) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[ReplicatedSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[ReplicatedSet[Int]]("b", network, mutable.Buffer("a"))

      val seta0 =
        given LocalUid = LocalUid.predefined(aea.replicaID)
        AntiEntropyContainer[ReplicatedSet[Int]](aea).mod(_.add(e1)).mod(_.add(e2))
      AntiEntropy.sync(aea, aeb)
      val setb0 = AntiEntropyContainer[ReplicatedSet[Int]](aeb).processReceivedDeltas()

      val seta1 = seta0.mod(_.add(using seta0.replicaID)(e))
      val setb1 = setb0.mod(_.clear())

      AntiEntropy.sync(aea, aeb)

      val seta2 = seta1.processReceivedDeltas()
      val setb2 = setb1.processReceivedDeltas()

      assert(
        seta2.data.elements == Set(e),
        s"Concurrently adding an element and clearing the set should result in a singleton set containing the added element, but ${seta2.data.elements} does not equal ${Set(e)}"
      )
      assert(
        setb2.data.elements == Set(e),
        s"Concurrently adding an element and clearing the set should result in a singleton set containing the added element, but ${setb2.data.elements} does not equal ${Set(e)}"
      )
    }
  }
  property("convergence") {
    forAll {
      (addedA: List[Int], removedA: List[Int], addedB: List[Int], removedB: List[Int], networkGen: NetworkGenerator) =>
        val network = networkGen.make()
        val aea     = new AntiEntropy[ReplicatedSet[Int]]("a", network, mutable.Buffer("b"))
        val aeb     = new AntiEntropy[ReplicatedSet[Int]]("b", network, mutable.Buffer("a"))

        val setaAdded = addedA.foldLeft(AntiEntropyContainer[ReplicatedSet[Int]](aea)) {
          case (set, e) => set.mod(_.add(using set.replicaID)(e))
        }
        val seta0 = removedA.foldLeft(setaAdded) {
          case (set, e) => set.mod(_.remove(e))
        }

        val setbAdded = addedB.foldLeft(AntiEntropyContainer[ReplicatedSet[Int]](aeb)) {
          case (set, e) => set.mod(_.add(using set.replicaID)(e))
        }
        val setb0 = removedB.foldLeft(setbAdded) {
          case (set, e) => set.mod(_.remove(e))
        }

        AntiEntropy.sync(aea, aeb)
        network.startReliablePhase()
        AntiEntropy.sync(aea, aeb)

        val seta1 = seta0.processReceivedDeltas()
        val setb1 = setb0.processReceivedDeltas()

        assert(
          seta1.data.elements == setb1.data.elements,
          s"After synchronization messages were reliably exchanged all replicas should converge, but ${seta1.data.elements} does not equal ${setb1.data.elements}"
        )
    }
  }
}
