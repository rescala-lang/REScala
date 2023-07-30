package test.kofre
import kofre.base.{Lattice, Uid}
import kofre.time.VectorClock
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.arbId
import test.kofre.baseproperties.{LatticePropertyChecks, bottomOption}
import test.kofre.DataGenerator.given

case class SomeProductType[A, B](paramA: A, paramB: B) derives Lattice

type NotInt = Int

given Arbitrary[SomeProductType[NotInt, NotInt]] = Arbitrary(for {
  as: NotInt <- Gen.posNum[Int]
  bs: NotInt <- Gen.posNum[Int]
} yield SomeProductType(as, bs))


class DerivedLattice extends LatticePropertyChecks[SomeProductType[NotInt, NotInt]]
