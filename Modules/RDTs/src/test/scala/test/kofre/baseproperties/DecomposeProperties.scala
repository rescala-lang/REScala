package test.kofre.baseproperties

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.alternatives.MultiValueRegister
import kofre.datatypes.contextual.{CausalQueue, LastWriterWins}
import kofre.datatypes.{GrowOnlyCounter, PosNegCounter}
import kofre.dotted.{Dotted, DottedLattice, HasDots}
import kofre.time.VectorClock
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.{*, given}

class GrowDecomposes       extends DecomposeProperties[GrowOnlyCounter]
class PosNegDecomposes     extends DecomposeProperties[PosNegCounter]
class TupleDecomposes      extends DecomposeProperties[(Set[Int], GrowOnlyCounter)]
class MultiValueDecomposes extends DecomposeProperties[MultiValueRegister[Int]]
class LWWDecomposes        extends LatticeMergeTest[Dotted[LastWriterWins[Int]]]
class LWWOptionDecomposes  extends DecomposeProperties[Dotted[Option[LastWriterWins[Int]]]]
class LWWTupleDecomposes extends DecomposeProperties[Dotted[(Option[LastWriterWins[Int]], Option[LastWriterWins[Int]])]]

abstract class DecomposeProperties[A: Arbitrary: Lattice: Bottom] extends LatticeMergeTest {

  override def scalaCheckInitialSeed = "e7t1GZf0uxItbW0JbGfq4uE1lIM8_DO-7l3DnhezSoC="

  property("decomposition") {
    forAll { (theValue: A) =>

      val decomposed = theValue.decomposed

      val empty = Bottom[A].empty

      val isDotted = theValue.isInstanceOf[Dotted[_]]

      decomposed.foreach { d =>
        assert(Lattice[A].lteq(d, theValue), s"decompose not smaller: »$d« <= »$theValue«\nmerge: ${d merge theValue}")
        assertNotEquals(empty, d, "decomposed result was empty")
        if isDotted
        then
          // do some extra checks which will cause failure later, but have better error reporting when done here
          decomposed.foreach: other =>
            if d != other
            then
              val thisCtx  = d.asInstanceOf[Dotted[_]].context
              val otherCtx = other.asInstanceOf[Dotted[_]].context
              assert(thisCtx disjunct otherCtx, s"overlapping context ${thisCtx} and ${otherCtx}")
      }

      assertEquals(empty merge theValue, Lattice.normalize(theValue), "bottom is bottom")

      val merged = decomposed.foldLeft(empty)(Lattice.merge)

      assertEquals(merged, Lattice.normalize(theValue), s"decompose does not recompose ${empty}")

    }
  }

}
