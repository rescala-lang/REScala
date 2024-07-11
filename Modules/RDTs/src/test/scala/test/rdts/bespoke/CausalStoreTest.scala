package test.rdts.bespoke

import rdts.base.Uid
import rdts.datatypes.experiments.{BoundedCounter, CausalDelta, CausalStore}
import rdts.dotted.Dotted
import rdts.syntax.TestReplica
import rdts.time.{ArrayRanges, Dot, Dots}
import test.rdts.DataGenerator.ExampleData
import test.rdts.given

import scala.language.implicitConversions

class CausalStoreTest extends munit.FunSuite {

  test("basic usage") {

    val a: Dotted[CausalStore[Map[Dot, ExampleData]]] =
      Dotted(
        CausalStore(
          CausalDelta(
            Dots(Map(Uid.predefined("c") -> ArrayRanges.elems(1))),
            Dots(Map()),
            Map(Dot("c", 1) -> "A pen Chris")
          ),
          Map(Dot("a", 2) -> "A in Anne")
        ),
        Dots.empty
      )

    val b: Dotted[CausalStore[Map[Dot, ExampleData]]] = Dotted(
      CausalStore(
        CausalDelta(
          Dots(Map(Uid.predefined("d") -> ArrayRanges.elems(3))),
          Dots(Map()),
          Map(Dot("d", 3) -> "B pen Erin")
        ),
        Map(Dot("g", 4) -> "B in Taylor")
      ),
      Dots(Map(Uid.predefined("d") -> ArrayRanges.elems(3), Uid.predefined("g") -> ArrayRanges.elems(4)))
    )

    val c: Dotted[CausalStore[Map[Dot, ExampleData]]] = Dotted(
      CausalStore(
        CausalDelta(
          Dots.empty,
          Dots.empty,
          Map.empty
        ),
        Map()
      ),
      Dots(Map(
        Uid.predefined("d") -> ArrayRanges.elems(3),
      ))
    )

    val ab = a `merge` b
    val bc = b `merge` c

    val ab_c = ab `merge` c
    val a_bc = a `merge` bc

    assertEquals(ab_c, a_bc)
  }
}
