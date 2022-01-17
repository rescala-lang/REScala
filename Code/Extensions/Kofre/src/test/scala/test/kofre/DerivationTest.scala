package test.kofre
import kofre.primitives.Version
import kofre.{IdUtil, Lattice}
import org.scalatest.freespec.AnyFreeSpec
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.arbId


case class SomeProductType[A, B](paramA: A, paramB: B) derives Lattice

opaque type NotInt = Int

given Arbitrary[SomeProductType[NotInt, NotInt]] = Arbitrary(for {
  as: NotInt <- Gen.posNum[Int]
  bs: NotInt <- Gen.posNum[Int]
} yield SomeProductType(as, bs))

given Lattice[NotInt] = math.max

class DerivedLattice extends LatticeMergeTest[SomeProductType[NotInt, NotInt]]
