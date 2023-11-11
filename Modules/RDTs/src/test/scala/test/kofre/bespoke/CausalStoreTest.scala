package test.kofre.bespoke

import kofre.base.Uid
import kofre.datatypes.experiments.{BoundedCounter, CausalDelta, CausalStore}
import kofre.dotted.{DotFun, Dotted}
import kofre.syntax.TestReplica
import kofre.time.{ArrayRanges, Dot, Dots}
import test.kofre.DataGenerator.ExampleData
import test.kofre.given

class CausalStoreTest extends munit.FunSuite {

  test("basic usage") {

    val a: Dotted[CausalStore[DotFun[ExampleData]]] =
      Dotted(
        CausalStore(
          CausalDelta(
            Dots(Map(Uid.predefined("c") -> ArrayRanges.elems(1))),
            Dots(Map()),
            DotFun(Map(Dot("c", 1) -> "A pen Chris"))
          ),
          DotFun(Map(Dot("a", 2) -> "A in Anne"))
        ),
        Dots.empty
      )

    val b: Dotted[CausalStore[DotFun[ExampleData]]] = Dotted(
      CausalStore(
        CausalDelta(
          Dots(Map(Uid.predefined("d") -> ArrayRanges.elems(3))),
          Dots(Map()),
          DotFun(Map(Dot("d", 3) -> "B pen Erin"))
        ),
        DotFun(Map(Dot("g", 4) -> "B in Taylor"))
      ),
      Dots(Map(Uid.predefined("d") -> ArrayRanges.elems(3), Uid.predefined("g") -> ArrayRanges.elems(4)))
    )

    val c: Dotted[CausalStore[DotFun[ExampleData]]] = Dotted(
      CausalStore(
        CausalDelta(
          Dots.empty,
          Dots.empty,
          DotFun.empty
        ),
        DotFun(Map())
      ),
      Dots(Map(
        Uid.predefined("d") -> ArrayRanges.elems(3),
      ))
    )

    val ab = a merge b
    val bc = b merge c

    val ab_c = ab merge c
    val a_bc = a merge bc

    assertEquals(ab_c, a_bc)
  }
}
