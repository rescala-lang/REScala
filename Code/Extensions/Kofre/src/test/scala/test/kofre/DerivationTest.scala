package test.kofre
import kofre.causality.VectorClock
import kofre.base.Lattice
import kofre.base.Defs
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.freespec.AnyFreeSpec
import test.kofre.DataGenerator.arbId

case class SomeProductType[A, B](paramA: A, paramB: B) derives Lattice

opaque type NotInt = Int

given Arbitrary[SomeProductType[NotInt, NotInt]] = Arbitrary(for {
  as: NotInt <- Gen.posNum[Int]
  bs: NotInt <- Gen.posNum[Int]
} yield SomeProductType(as, bs))

given Lattice[NotInt] = math.max _

class DerivedLattice extends LatticeMergeTest[SomeProductType[NotInt, NotInt]]
