package test.kofre

import kofre.datatypes.alternatives.MultiValueRegister
import kofre.time.VectorClock
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.arbVectorClock

import scala.Console.in
import scala.collection.Seq

class ParallelVersionTest extends munit.ScalaCheckSuite {

  given Arbitrary[List[VectorClock]] = Arbitrary(Gen.nonEmptyListOf(arbVectorClock.arbitrary))

  property("works") {
    forAll { (a: List[VectorClock]) =>
      val rem = MultiValueRegister.parallelVersionSubset(a, Nil)
      if a.nonEmpty then assert(rem.nonEmpty, "should keep some version")

      val thrownAway = a.filterNot(rem.contains)
      thrownAway.foreach(g => assert(rem.exists(r => g <= r), "removed must be subsumed"))
      rem.foreach(r => rem.foreach(a => assert(!(a < r), "remaining must be incomparable")))
    }
  }
}
