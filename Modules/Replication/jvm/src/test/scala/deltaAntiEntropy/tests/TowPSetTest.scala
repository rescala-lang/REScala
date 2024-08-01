package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyContainer, Network}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import rdts.datatypes.TwoPhaseSet
import replication.JsoniterCodecs.given

import scala.collection.mutable

object TwoPSetGenerators {
  def genTwoPSet[E: Arbitrary](using c: JsonValueCodec[E]): Gen[AntiEntropyContainer[TwoPhaseSet[E]]] =
    for
      added   <- Gen.containerOf[List, E](Arbitrary.arbitrary[E])
      n       <- Gen.choose(0, added.size)
      removed <- Gen.pick(n, added)
    yield {
      val network = new Network(0, 0, 0)
      val ae =
        new AntiEntropy[TwoPhaseSet[E]]("a", network, mutable.Buffer())(using summon, twoPSetContext[E], summon)
      val setAdded = added.foldLeft(AntiEntropyContainer[TwoPhaseSet[E]](ae)) {
        case (set, e) => set.modn(_.insert(e))
      }
      removed.foldLeft(setAdded) {
        case (set, e) => set.modn(_.remove(e))
      }
    }

  given arbTwoPSet[E: Arbitrary](using
      c: JsonValueCodec[E]
  ): Arbitrary[AntiEntropyContainer[TwoPhaseSet[E]]] =
    Arbitrary(genTwoPSet)
}

class TowPSetTest extends munit.ScalaCheckSuite {
  import TwoPSetGenerators.{*, given}

  given intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  property("insert") {
    forAll { (insert: List[Int], remove: List[Int], e: Int) =>
      val network = new Network(0, 0, 0)
      val ae      = new AntiEntropy[TwoPhaseSet[Int]]("a", network, mutable.Buffer())

      val setInserted = insert.foldLeft(AntiEntropyContainer[TwoPhaseSet[Int]](ae)) {
        case (s, e) => s.modn(_.insert(e))
      }

      val set = remove.foldLeft(setInserted) {
        case (s, e) => s.modn(_.remove(e))
      }

      val setNewInsert = set.modn(_.insert(e))

      assert(
        remove.contains(e) || setNewInsert.data.elements.contains(e),
        s"After adding an element that was never before removed the set should contain this element, but ${setNewInsert.data.elements} does not contain $e"
      )
    }
  }
  property("remove") {
    forAll { (set: AntiEntropyContainer[TwoPhaseSet[Int]], e: Int) =>
      val removed = set.modn(_.remove(e))

      assert(
        !removed.data.elements.contains(e),
        s"After removing an element it should no longer be contained in the set, but ${removed.data.elements} contains $e"
      )
    }
  }
  property("concurrent insert/remove") {
    forAll { (addOrRemoveA: Either[Int, Int], addOrRemoveB: Either[Int, Int]) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[TwoPhaseSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[TwoPhaseSet[Int]]("b", network, mutable.Buffer("a"))

      val sa0 = addOrRemoveA match {
        case Left(e)  => AntiEntropyContainer[TwoPhaseSet[Int]](aea).modn(_.insert(e))
        case Right(e) => AntiEntropyContainer[TwoPhaseSet[Int]](aea).modn(_.remove(e))
      }
      val sb0 = addOrRemoveB match {
        case Left(e)  => AntiEntropyContainer[TwoPhaseSet[Int]](aeb).modn(_.insert(e))
        case Right(e) => AntiEntropyContainer[TwoPhaseSet[Int]](aeb).modn(_.remove(e))
      }

      AntiEntropy.sync(aea, aeb)

      val sa1 = sa0.processReceivedDeltas()
      val sb1 = sb0.processReceivedDeltas()

      val sequential = addOrRemoveB match {
        case Left(e)  => sa0.modn(_.insert(e))
        case Right(e) => sa0.modn(_.remove(e))
      }

      assert(
        sa1.data.elements == sequential.data.elements,
        s"Concurrent execution of insertGL/remove should be equivalent to their sequential execution, but ${sa1.data.elements} does not equal ${sequential.data.elements}"
      )
      assert(
        sb1.data.elements == sequential.data.elements,
        s"Concurrent execution of insertGL/remove should be equivalent to their sequential execution, but ${sb1.data.elements} does not equal ${sequential.data.elements}"
      )
    }
  }
  property("convergence") {
    forAll {
      (insertA: List[Int], removeA: List[Int], insertB: List[Int], removeB: List[Int], networkGen: NetworkGenerator) =>
        val network = networkGen.make()
        val aea     = new AntiEntropy[TwoPhaseSet[Int]]("a", network, mutable.Buffer("b"))
        val aeb     = new AntiEntropy[TwoPhaseSet[Int]]("b", network, mutable.Buffer("a"))

        val insertedA = insertA.foldLeft(AntiEntropyContainer[TwoPhaseSet[Int]](aea)) {
          case (s, e) => s.modn(_.insert(e))
        }
        val sa0 = removeA.foldLeft(insertedA) {
          case (s, e) => s.modn(_.remove(e))
        }
        val insertedB = insertB.foldLeft(AntiEntropyContainer[TwoPhaseSet[Int]](aeb)) {
          case (s, e) => s.modn(_.insert(e))
        }
        val sb0 = removeB.foldLeft(insertedB) {
          case (s, e) => s.modn(_.remove(e))
        }

        AntiEntropy.sync(aea, aeb)
        network.startReliablePhase()
        AntiEntropy.sync(aea, aeb)

        val sa1 = sa0.processReceivedDeltas()
        val sb1 = sb0.processReceivedDeltas()

        assert(
          sa1.data.elements == sb1.data.elements,
          s"After synchronization messages were reliably exchanged all replicas should converge, but ${sa1.data.elements} does not equal ${sb1.data.elements}"
        )
    }
  }
}
