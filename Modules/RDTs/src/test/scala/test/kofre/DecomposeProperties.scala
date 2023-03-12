package test.kofre

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.alternatives.MultiValueRegister
import kofre.datatypes.{CausalQueue, GrowOnlyCounter, LastWriterWins, PosNegCounter}
import kofre.dotted.Dotted
import kofre.time.VectorClock
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.{*, given}

class GrowDecomposes       extends DecomposeProperties[GrowOnlyCounter]
class PosNegDecomposes     extends DecomposeProperties[PosNegCounter]
class TupleDecomposes      extends DecomposeProperties[(Set[Int], GrowOnlyCounter)]
class MultiValueDecomposes extends DecomposeProperties[MultiValueRegister[Int]]
class LWWDecomposes        extends DecomposeProperties[Dotted[Option[LastWriterWins[Int]]]]

// this may fail in cases where both LWWs have the same dot generated
class LWWTupleDecomposes extends DecomposeProperties[Dotted[(Option[LastWriterWins[Int]], Option[LastWriterWins[Int]])]]

abstract class DecomposeProperties[A: Arbitrary: Lattice: Bottom] extends munit.ScalaCheckSuite {

  test("decomposition") {
    forAll { (theValue: A) =>

      val decomposed = theValue.decomposed

      val empty = Bottom[A].empty

      decomposed.foreach { d =>
        assert(Lattice[A].lteq(d, theValue), s"decompose not smaller: »$d« <= »$theValue«\nmerge: ${d merge theValue}")
        assertNotEquals(empty, d, "decomposed result was empty")
      }

      assertEquals(empty merge theValue, theValue, "bottom is bottom")

      val merged = decomposed.foldLeft(empty)(Lattice.merge)

      assertEquals(merged, Lattice.normalize(theValue), s"decompose does not recompose ${empty}")

    }
  }

}
