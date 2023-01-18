package test.kofre

import kofre.base.{Bottom, DecomposeLattice, Lattice}
import kofre.datatypes.alternatives.MultiValueRegister
import kofre.datatypes.experiments.CausalQueue
import kofre.time.VectorClock
import kofre.datatypes.{GrowOnlyCounter, LastWriterWins, PosNegCounter}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.{*, given}

class GrowDecomposes   extends DecomposeProperties[GrowOnlyCounter]
class PosNegDecomposes extends DecomposeProperties[PosNegCounter]
class TupleDecomposes  extends DecomposeProperties[(Set[Int], GrowOnlyCounter)]
class MultiValueDecomposes  extends DecomposeProperties[MultiValueRegister[Int]]

abstract class DecomposeProperties[A: Arbitrary: DecomposeLattice: Bottom] extends munit.ScalaCheckSuite {

  test("decomposition") {
    forAll { (counter: A) =>

      val decomposed = counter.decomposed

      val empty = Bottom[A].empty

      decomposed.foreach { d =>
        assert(Lattice[A].lteq(d, counter), s"decompose not smaller: »$d« <= »$counter«\nmerge: ${d merge counter}")
        assertNotEquals(empty, d, "decomposed result was empty")
      }

      val merged = counter.decomposed.foldLeft(empty)(Lattice.merge)

      assertEquals(merged, Lattice.normalize(counter), "decompose does not recompose")

    }
  }

}
