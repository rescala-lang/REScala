package test.kofre

import kofre.base.{DecomposeLattice, Defs, Lattice}
import kofre.causality.VectorClock
import kofre.predef.{GrowOnlyCounter, PosNegCounter}
import kofre.primitives.{CausalQueue, LastWriterWins, MultiValueRegister}
import kofre.sets.ORSet
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.{*, given}

class GrowDecomposes   extends DecomposeProperties[GrowOnlyCounter]
class PosNegDecomposes extends DecomposeProperties[PosNegCounter]
class TupleDecomposes  extends DecomposeProperties[(Set[Int], GrowOnlyCounter)]

abstract class DecomposeProperties[A: Arbitrary: DecomposeLattice] extends munit.ScalaCheckSuite {

  test("decomposition") {
    forAll { (counter: A) =>

      val decomposed = counter.decomposed

      val empty = DecomposeLattice[A].empty

      decomposed.foreach { d =>
        assert(Lattice[A].lteq(d, counter), "decompose not smaller")
        assertNotEquals(empty, d, "decomposed result was empty")
      }

      val merged = counter.decomposed.foldLeft(empty)(Lattice.merge)

      assertEquals(merged, counter, "decompose does not recompose")

    }
  }

}
