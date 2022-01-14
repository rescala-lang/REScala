package tests.distribution.delta.crdt.basic

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.github.plokhotnyuk.jsoniter_scala.macros.JsonCodecMaker
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.extra.lattices.delta.Codecs._
import rescala.extra.lattices.delta.crdt.basic._
import tests.distribution.delta.crdt.basic.NetworkGenerators._

import scala.collection.mutable

object TwoPSetGenerators {
  def genTwoPSet[E: Arbitrary](implicit c: JsonValueCodec[E]): Gen[TwoPSet[E]] = for {
    added   <- Gen.containerOf[List, E](Arbitrary.arbitrary[E])
    n       <- Gen.choose(0, added.size)
    removed <- Gen.pick(n, added)
  } yield {
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropyImpl[TwoPSet.State[E]]("a", network, mutable.Buffer())
    val setAdded = added.foldLeft(TwoPSet(ae)) {
      case (set, e) => set.insert(e)
    }
    removed.foldLeft(setAdded) {
      case (set, e) => set.remove(e)
    }
  }

  implicit def arbTwoPSet[E: Arbitrary](implicit c: JsonValueCodec[E]): Arbitrary[TwoPSet[E]] = Arbitrary(genTwoPSet)
}

class TowPSetTest extends AnyFreeSpec with ScalaCheckDrivenPropertyChecks {
  import TwoPSetGenerators._

  implicit val intCodec: JsonValueCodec[Int] = JsonCodecMaker.make

  "insert" in forAll { (insert: List[Int], remove: List[Int], e: Int) =>
    val network = new Network(0, 0, 0)
    val ae      = new AntiEntropyImpl[TwoPSet.State[Int]]("a", network, mutable.Buffer())

    val setInserted = insert.foldLeft(TwoPSet[Int](ae)) {
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

  "remove" in forAll { (set: TwoPSet[Int], e: Int) =>
    val removed = set.remove(e)

    assert(
      !removed.elements.contains(e),
      s"After removing an element it should no longer be contained in the set, but ${removed.elements} contains $e"
    )
  }

  "concurrent insert/remove" in forAll { (addOrRemoveA: Either[Int, Int], addOrRemoveB: Either[Int, Int]) =>
    val network = new Network(0, 0, 0)

    val aea = new AntiEntropyImpl[TwoPSet.State[Int]]("a", network, mutable.Buffer("b"))
    val aeb = new AntiEntropyImpl[TwoPSet.State[Int]]("b", network, mutable.Buffer("a"))

    val sa0 = addOrRemoveA match {
      case Left(e)  => TwoPSet(aea).insert(e)
      case Right(e) => TwoPSet(aea).remove(e)
    }
    val sb0 = addOrRemoveB match {
      case Left(e)  => TwoPSet(aeb).insert(e)
      case Right(e) => TwoPSet(aeb).remove(e)
    }

    AntiEntropyImpl.sync(aea, aeb)

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

  "convergence" in forAll {
    (insertA: List[Int], removeA: List[Int], insertB: List[Int], removeB: List[Int], network: Network) =>
      val aea = new AntiEntropyImpl[TwoPSet.State[Int]]("a", network, mutable.Buffer("b"))
      val aeb = new AntiEntropyImpl[TwoPSet.State[Int]]("b", network, mutable.Buffer("a"))

      val insertedA = insertA.foldLeft(TwoPSet[Int](aea)) {
        case (s, e) => s.insert(e)
      }
      val sa0 = removeA.foldLeft(insertedA) {
        case (s, e) => s.remove(e)
      }
      val insertedB = insertB.foldLeft(TwoPSet[Int](aeb)) {
        case (s, e) => s.insert(e)
      }
      val sb0 = removeB.foldLeft(insertedB) {
        case (s, e) => s.remove(e)
      }

      AntiEntropyImpl.sync(aea, aeb)
      network.startReliablePhase()
      AntiEntropyImpl.sync(aea, aeb)

      val sa1 = sa0.processReceivedDeltas()
      val sb1 = sb0.processReceivedDeltas()

      assert(
        sa1.elements == sb1.elements,
        s"After synchronization messages were reliably exchanged all replicas should converge, but ${sa1.elements} does not equal ${sb1.elements}"
      )
  }
}
