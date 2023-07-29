package test.kofre.baseproperties

import kofre.base.{Bottom, Lattice}
import kofre.datatypes.alternatives.MultiValueRegister
import kofre.datatypes.alternatives.lww.CausalLastWriterWins
import kofre.datatypes.contextual.CausalQueue
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
class LWWDecomposes        extends DecomposeProperties[CausalLastWriterWins[Int]]
class LWWOptionDecomposes  extends DecomposeProperties[Option[CausalLastWriterWins[Int]]]
class LWWTupleDecomposes
    extends DecomposeProperties[(Option[CausalLastWriterWins[Int]], Option[CausalLastWriterWins[Int]])]


private inline given bottomOption[A]: Option[Bottom[A]] =
  scala.compiletime.summonFrom:
    case b: Bottom[A] => Some(b)
    case _ => None

abstract class DecomposeProperties[A: Arbitrary: Lattice](using bot: Option[Bottom[A]]) extends LatticeMergeTest {

  val empty = bot.map(_.empty)

  property("decomposition") {
    forAll { (theValue: A) =>

      val decomposed = theValue.decomposed

      val isDotted = theValue.isInstanceOf[Dotted[_]]

      decomposed.foreach { d =>
        assert(Lattice[A].lteq(d, theValue), s"decompose not smaller: »$d« <= »$theValue«\nmerge: ${d merge theValue}")
        empty match
          case Some(empty) => assertNotEquals(empty, d, "decomposed result was empty")
          case other =>
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

      empty match
        case Some(empty) => assertEquals(empty merge theValue, Lattice.normalize(theValue), "bottom is bottom")
        case other =>


      val merged = decomposed.reduceLeftOption(Lattice.merge)

      assertEquals(merged.orElse(empty), Some(Lattice.normalize(theValue)), s"decompose does not recompose")

    }
  }

}
