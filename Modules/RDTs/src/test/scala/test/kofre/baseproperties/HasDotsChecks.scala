package test.kofre.baseproperties

import kofre.base.Uid
import kofre.datatypes.contextual.ReplicatedList
import kofre.datatypes.experiments.CausalStore
import kofre.dotted.{DotFun, Dotted, HasDots}
import kofre.time.{ArrayRanges, Dots, VectorClock}
import org.scalacheck.Prop.*
import org.scalacheck.{Arbitrary, Gen}
import test.kofre.DataGenerator.{*, given}
import test.kofre.DataGenerator.RGAGen.given

import scala.collection.immutable.Queue
import scala.math.Ordering.Implicits.infixOrderingOps

class DotSetHDChecks         extends HasDotsChecks[Dots]
class CausalStoreHDChecks    extends HasDotsChecks[CausalStore[DotFun[ExampleData]]]
class ReplicatedListHDChecks extends HasDotsChecks[ReplicatedList[ExampleData]]

// the specification of these tests is nice, but the generators are essentially useless, as it is extremely unlikely
// that they will produce any kind of comparable values
abstract class HasDotsChecks[A: Arbitrary: HasDots] extends munit.ScalaCheckSuite {
  property("remove is precise") {
    forAll: (a: A) =>
      val dots = a.dots
      dots.iterator.foreach: dot =>
        val single = Dots.single(dot)
        val should = dots subtract single
        val remaining = a.removeDots(single) match
          case None        => Dots.empty
          case Some(value) => value.dots

        assertEquals(remaining, should, s"removed $single")
  }

  property("remove is exhaustive") {
    forAll: (a: A) =>
      a.removeDots(a.dots) match
        case Some(value) => assertEquals(value.dots, Dots.empty)
        case None        => // good
  }

}
