package deltaAntiEntropy.tests

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import deltaAntiEntropy.tests.NetworkGenerators.*
import deltaAntiEntropy.tools.{AntiEntropy, AntiEntropyCRDT}
import kofre.datatypes.TwoPhaseSet
import kofre.decompose.containers.Network
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import replication.JsoniterCodecs.*

import scala.collection.mutable

object TwoPSetGenerators {
  def genTwoPSet[E: Arbitrary](implicit c: JsonValueCodec[E]): Gen[AntiEntropyCRDT[TwoPhaseSet[E]]] = for {
    added   <- Gen.containerOf[List, E](Arbitrary.arbitrary[E])
    n       <- Gen.choose(0, added.size)
    removed <- Gen.pick(n, added)
  } yield {
    val network = new Network(0, 0, 0)
    val ae = new AntiEntropy[TwoPhaseSet[E]]("a", network, mutable.Buffer())(implicitly, twoPSetContext[E], implicitly)
    val setAdded = added.foldLeft(AntiEntropyCRDT[TwoPhaseSet[E]](ae)) {
      case (set, e) => set.insert(e)
    }
    removed.foldLeft(setAdded) {
      case (set, e) => set.remove(e)
    }
  }

  implicit def arbTwoPSet[E: Arbitrary](implicit c: JsonValueCodec[E]): Arbitrary[AntiEntropyCRDT[TwoPhaseSet[E]]] =
    Arbitrary(genTwoPSet)
}

class TowPSetTest extends munit.ScalaCheckSuite {
  import TwoPSetGenerators.*

  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make
  property("insert") {
    forAll { (insert: List[Int], remove: List[Int], e: Int) =>
      val network = new Network(0, 0, 0)
      val ae      = new AntiEntropy[TwoPhaseSet[Int]]("a", network, mutable.Buffer())

      val setInserted = insert.foldLeft(AntiEntropyCRDT[TwoPhaseSet[Int]](ae)) {
        case (s, e) => s.insert(e)
      }

      val set = remove.foldLeft(setInserted) {
        case (s, e) => s.remove(e)
      }

      val setNewInsert = set.insert(e)

      assert(
        remove.contains(e) || setNewInsert.elements.contains(e),
        s"After adding an element that was never before removed the set should contain this element, but ${setNewInsert.elements} does not contain $e"
      )
    }
  }
  property("remove") {
    forAll { (set: AntiEntropyCRDT[TwoPhaseSet[Int]], e: Int) =>
      val removed = set.remove(e)

      assert(
        !removed.elements.contains(e),
        s"After removing an element it should no longer be contained in the set, but ${removed.elements} contains $e"
      )
    }
  }
  property("concurrent insert/remove") {
    forAll { (addOrRemoveA: Either[Int, Int], addOrRemoveB: Either[Int, Int]) =>
      val network = new Network(0, 0, 0)

      val aea = new AntiEntropy[TwoPhaseSet[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropy[TwoPhaseSet[Int]]("b", network, mutable.Buffer("a"))

      val sa0 = addOrRemoveA match {
        case Left(e)  => AntiEntropyCRDT[TwoPhaseSet[Int]](aea).insert(e)
        case Right(e) => AntiEntropyCRDT[TwoPhaseSet[Int]](aea).remove(e)
      }
      val sb0 = addOrRemoveB match {
        case Left(e)  => AntiEntropyCRDT[TwoPhaseSet[Int]](aeb).insert(e)
        case Right(e) => AntiEntropyCRDT[TwoPhaseSet[Int]](aeb).remove(e)
      }

      AntiEntropy.sync(aea, aeb)

      val sa1 = sa0.processReceivedDeltas()
      val sb1 = sb0.processReceivedDeltas()

      val sequential = addOrRemoveB match {
        case Left(e)  => sa0.insert(e)
        case Right(e) => sa0.remove(e)
      }

      assert(
        sa1.elements == sequential.elements,
        s"Concurrent execution of insert/remove should be equivalent to their sequential execution, but ${sa1.elements} does not equal ${sequential.elements}"
      )
      assert(
        sb1.elements == sequential.elements,
        s"Concurrent execution of insert/remove should be equivalent to their sequential execution, but ${sb1.elements} does not equal ${sequential.elements}"
      )
    }
  }
  property("convergence") {
    forAll {
      (insertA: List[Int], removeA: List[Int], insertB: List[Int], removeB: List[Int], network: Network) =>
        val aea = new AntiEntropy[TwoPhaseSet[Int]]("a", network, mutable.Buffer("b"))
        val aeb = new AntiEntropy[TwoPhaseSet[Int]]("b", network, mutable.Buffer("a"))

        val insertedA = insertA.foldLeft(AntiEntropyCRDT[TwoPhaseSet[Int]](aea)) {
          case (s, e) => s.insert(e)
        }
        val sa0 = removeA.foldLeft(insertedA) {
          case (s, e) => s.remove(e)
        }
        val insertedB = insertB.foldLeft(AntiEntropyCRDT[TwoPhaseSet[Int]](aeb)) {
          case (s, e) => s.insert(e)
        }
        val sb0 = removeB.foldLeft(insertedB) {
          case (s, e) => s.remove(e)
        }

        AntiEntropy.sync(aea, aeb)
        network.startReliablePhase()
        AntiEntropy.sync(aea, aeb)

        val sa1 = sa0.processReceivedDeltas()
        val sb1 = sb0.processReceivedDeltas()

        assert(
          sa1.elements == sb1.elements,
          s"After synchronization messages were reliably exchanged all replicas should converge, but ${sa1.elements} does not equal ${sb1.elements}"
        )
    }
  }
}
