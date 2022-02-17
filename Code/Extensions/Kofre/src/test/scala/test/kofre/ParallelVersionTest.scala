package test.kofre

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import kofre.primitives.{MultiValueRegister}


import scala.Console.in
import scala.collection.Seq
import test.kofre.DataGenerator.arbVersion

import kofre.causality.VectorClock

class ParallelVersionTest extends AnyFreeSpec, ScalaCheckDrivenPropertyChecks {

  given Arbitrary[List[VectorClock]] = Arbitrary(Gen.nonEmptyListOf(arbVersion.arbitrary))

  "works" in forAll { (a: List[VectorClock]) =>
    val rem = MultiValueRegister.parallelVersionSubset(a, Nil)
    if a.nonEmpty then assert(rem.nonEmpty, "should keep some version")

    val thrownAway = a.filterNot(rem.contains)
    thrownAway.foreach(g => assert(rem.exists(r => g <= r), "removed must be subsumed"))
    rem.foreach(r => rem.foreach(a => assert(!(a < r), "remaining must be incomparable")))
  }

}
