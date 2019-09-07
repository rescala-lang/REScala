package tests.distribution

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.FreeSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import rescala.lattices.Lattice
import rescala.lattices.primitives.GCounter

object DataGenerator {
  implicit val genCounter: Arbitrary[GCounter] = Arbitrary(for {
    ids <- Gen.listOf(Gen.oneOf('a' to 'g').map(_.toString)).map(_.toSet)
    if ids.nonEmpty
    value <- Gen.listOfN(ids.size, Gen.oneOf(0 to 100))
    id <- Gen.oneOf(ids.toSeq)
  } yield GCounter(id, scala.collection.immutable.HashMap(ids.zip(value).toSeq : _*)))
}

class CounterTest extends FreeSpec with ScalaCheckDrivenPropertyChecks {

  import tests.distribution.DataGenerator._


  "idempotent" in forAll { (a: GCounter, b: GCounter) =>
    val ab = Lattice.merge(a, b)
    val abb = Lattice.merge(ab, b)
    assert(ab.value === abb.value)

  }

  "commutative" in forAll { (a: GCounter, b: GCounter) =>
    assert(Lattice.merge(b, a).value === Lattice.merge(a, b).value)
  }

  "associative" in forAll { (a: GCounter, b: GCounter, c: GCounter) =>
    val ab = Lattice.merge(a, b)
    val bc = Lattice.merge(b, c)
    val abc = Lattice.merge(ab, c)
    val abc2 = Lattice.merge(a, bc)
    assert(abc.value === abc2.value)
  }

}
