package test.rdts.bespoke

import org.scalacheck.{Arbitrary, Gen}
import rdts.base.{Bottom, Lattice, Uid}
import rdts.time.VectorClock
import test.rdts.DataGenerator.{arbId, given}
import test.rdts.baseproperties.LatticePropertyChecks

case class SomeProductType[A, B](paramA: A, paramB: B) derives Lattice

type NotInt = Int

given Arbitrary[SomeProductType[NotInt, NotInt]] = Arbitrary(for
  as: NotInt <- Gen.posNum[Int]
  bs: NotInt <- Gen.posNum[Int]
yield SomeProductType(as, bs))

class DerivedLattice extends LatticePropertyChecks[SomeProductType[NotInt, NotInt]]

enum TestEnum:
  case A
  case B
  case C(x: Int)
object TestEnum:
  given Lattice[TestEnum] =
    given Lattice[TestEnum.A.type] = Lattice.derived
    given Lattice[TestEnum.B.type] = Lattice.derived
    given Lattice[TestEnum.C]      = Lattice.derived
    Lattice.sumLattice
  given Bottom[TestEnum] =
    given Bottom[TestEnum.A.type] = Bottom.derived
    Bottom.derived

  given Arbitrary[TestEnum] = Arbitrary:
    Arbitrary.arbitrary[Int].flatMap: i =>
      Gen.oneOf(TestEnum.A, TestEnum.B, TestEnum.C(i))

class EnumLatticeChecks extends LatticePropertyChecks[TestEnum]:
  test("custom enum lattice tests"):

    import TestEnum.*
    assertEquals(A `merge` B, B)
    assertEquals(C(1) `merge` B `merge` A, C(1))
    assertEquals((C(10) `merge` A) `merge` (C(100) `merge` B), C(100))
