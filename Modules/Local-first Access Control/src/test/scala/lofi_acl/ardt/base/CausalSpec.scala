package lofi_acl.ardt.base

import lofi_acl.ardt.base.Causal.given
import lofi_acl.ardt.base.StandardLibrary.GrowOnlySet.given
import lofi_acl.ardt.base.{Causal, Lattice}
import lofi_acl.ardt.causality.DotStore.{DotFun, DotSet}
import munit.FunSuite
import rdts.time.{Dot, Dots}

class CausalSpec extends FunSuite {
  def dot(time: Long, replicaId: String): Dot = Dot(replicaId, time)

  import scala.language.implicitConversions

  private given mapToSetOfDots: Conversion[Map[String, Int], Set[Dot]] =
    _.flatMap(tuple =>
      1 to tuple._2 map {
        Dot(tuple._1, _)
      }
    ).toSet

  private given setOfDotsToDotSet: Conversion[Set[Dot], DotSet] = Dots.from(_)

  test("Causal with DotSet should merge when empty") {
    assertEquals(
      Lattice[Causal[DotSet]].merge(
        Causal.bottom[DotSet],
        Causal.bottom[DotSet]
      ),
      Causal.bottom[DotSet]
    )
  }

  test("Causal with DotSet should union the CausalContext") {
    assertEquals(
      Lattice[Causal[DotSet]].merge(
        Causal(Set.empty[Dot], Dots.from(Map("A" -> 1))),
        Causal(Set.empty[Dot], Dots.from(Map("B" -> 2)))
      ),
      Causal[DotSet](Set.empty[Dot], Dots.from(Map("A" -> 1, "B" -> 2)))
    )

    assertEquals(
      Lattice[Causal[DotSet]].merge(
        Causal(Set.empty[Dot], Dots.from(Map("A" -> 1, "B" -> 1))),
        Causal(Set.empty[Dot], Dots.from(Map("B" -> 2)))
      ),
      Causal[DotSet](Set.empty[Dot], Dots.from(Map("A" -> 1, "B" -> 2)))
    )
  }

  test("Causal with DotSet should merge with disjoint dotset") {
    assertEquals(
      Lattice[Causal[DotSet]].merge(
        Causal(Set(dot(1, "A")), Dots.from(Map("A" -> 1))),
        Causal(Set(dot(1, "B")), Dots.from(Map("B" -> 1)))
      ),
      Causal[DotSet](Set(dot(1, "A"), dot(1, "B")), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  test("Causal with DotSet should remove dots from dotset that are contained in CausalContext") {
    assertEquals(
      Lattice[Causal[DotSet]].merge(
        Causal(Set(dot(1, "A")), Dots.from(Map("A" -> 1))),
        Causal(Set(dot(2, "A")), Dots.from(Map("A" -> 2)))
      ),
      Causal[DotSet](
        Set(dot(2, "A")),
        Dots.from(Map("A" -> 2))
      )
    )
  }

  test("Causal with DotSet should remove dots from dotset if removed") {
    assertEquals(
      Lattice[Causal[DotSet]].merge(
        Causal(Set(dot(2, "A")), Dots.from(Map("A" -> 2))),
        Causal(Set.empty[Dot], Dots.from(Map("A" -> 2)))
      ),
      Causal[DotSet](
        Set.empty[Dot],
        Dots.from(Map("A" -> 2))
      )
    )

    assertEquals(
      Lattice[Causal[DotSet]].merge(
        Causal(Set.empty[Dot], Dots.from(Map("A" -> 2))),
        Causal(Set(dot(2, "A")), Dots.from(Map("A" -> 2)))
      ),
      Causal[DotSet](
        Set.empty[Dot],
        Dots.from(Map("A" -> 2))
      )
    )

    assertEquals(
      Lattice[Causal[DotSet]].merge(
        Causal(Set(dot(1, "B")), Dots.from(Map("A" -> 2, "B" -> 1))),
        Causal(Set(dot(2, "A")), Dots.from(Map("A" -> 2)))
      ),
      Causal[DotSet](
        Set(dot(1, "B")),
        Dots.from(Map("A" -> 2, "B" -> 1))
      )
    )
  }

  test("Causal with DotFun should merge when empty") {
    assertEquals(
      Lattice[Causal[DotFun[Set[Int]]]].merge(
        Causal(Map(), Dots.empty),
        Causal(Map(), Dots.empty)
      ),
      Causal[DotFun[Set[Int]]](Map(), Dots.empty)
    )
  }

  test("Causal with DotFun should merge idempotent") {
    assertEquals(
      Lattice[Causal[DotFun[Set[Int]]]].merge(
        Causal(Map(dot(1, "A") -> Set(1)), Dots.from(Map("A" -> 1))),
        Causal(Map(dot(1, "A") -> Set(1)), Dots.from(Map("A" -> 1)))
      ),
      Causal(Map(dot(1, "A") -> Set(1)), Dots.from(Map("A" -> 1)))
    )

    assertEquals(
      Lattice[Causal[DotFun[Set[Int]]]].merge(
        Causal(Map(), Dots.from(Map("A" -> 2))),
        Causal(Map(), Dots.from(Map("A" -> 2)))
      ),
      Causal(Map[Dot, Set[Int]](), Dots.from(Map("A" -> 2)))
    )

    assertEquals(
      Lattice[Causal[DotFun[Set[Int]]]].merge(
        Causal(Map(dot(1, "A") -> Set()), Dots.from(Map("A" -> 2))),
        Causal(Map(dot(1, "A") -> Set()), Dots.from(Map("A" -> 2)))
      ),
      Causal(Map(dot(1, "A") -> Set()), Dots.from(Map("A" -> 2)))
    )
  }

  test("Causal with DotFun should merge lattice when dot contained in both dotstores") {
    assertEquals(
      Lattice[Causal[DotFun[Set[Int]]]].merge(
        Causal(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1))),
        Causal(Map(dot(1, "A") -> Set(21)), Dots.from(Map("A" -> 1)))
      ),
      Causal(
        Map(
          dot(1, "A") -> Set(21, 42)
        ),
        Dots.from(Map("A" -> 1))
      )
    )
  }

  test("Causal with DotFun should union dotstore when disjoint dotstores and not a causal removal") {
    assertEquals(
      Lattice[Causal[DotFun[Set[Int]]]].merge(
        Causal(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1))),
        Causal(Map(dot(1, "B") -> Set(21)), Dots.from(Map("B" -> 1)))
      ),
      Causal(
        Map(
          dot(1, "A") -> Set(42),
          dot(1, "B") -> Set(21)
        ),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )
  }

  test(
    "Causal with DotFun should discard values in dotstore when not contained in causal context and has disjoint dotstores"
  ) {
    assertEquals(
      Lattice[Causal[DotFun[Set[Int]]]].merge(
        Causal(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1))),
        Causal(Map(dot(1, "B") -> Set(21)), Dots.from(Map("A" -> 1, "B" -> 1)))
      ),
      Causal(
        Map(
          dot(1, "B") -> Set(21)
        ),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )

    assertEquals(
      Lattice[Causal[DotFun[Set[Int]]]].merge(
        Causal(Map(dot(1, "B") -> Set(21)), Dots.from(Map("A" -> 1, "B" -> 1))),
        Causal(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1)))
      ),
      Causal(
        Map(
          dot(1, "B") -> Set(21)
        ),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )
  }

  test("Causal with DotMap should merge when empty") {
    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](Map(), Dots.empty),
        Causal[Map[Int, DotSet]](Map(), Dots.empty)
      ),
      Causal[Map[Int, DotSet]](Map(), Dots.empty)
    )
  }

  test("Causal with DotMap should merge idempotent") {
    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
        Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1)))
      ),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1)))
    )
  }

  test("Causal with DotMap should remove values when removal is causal") {
    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
        Causal[Map[Int, DotSet]](Map(), Dots.from(Map("A" -> 2)))
      ),
      Causal[Map[Int, DotSet]](Map(), Dots.from(Map("A" -> 2)))
    )

    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](Map(), Dots.from(Map("A" -> 2))),
        Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1)))
      ),
      Causal[Map[Int, DotSet]](Map(), Dots.from(Map("A" -> 2)))
    )
  }

  test("Causal with DotMap should replace dotstore when causal") {
    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
        Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "B"))), Dots.from(Map("A" -> 1, "B" -> 1)))
      ),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "B"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  test("Causal with DotMap should merge dotstore when disjoint") {
    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
        Causal[Map[Int, DotSet]](Map(2 -> Set(dot(1, "B"))), Dots.from(Map("B" -> 1)))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )
  }

  test("Causal with DotMap should merge dotstore when not disjoint") {
    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](
          Map(1 -> Set(dot(1, "A"))),
          Dots.from(Map("A" -> 1))
        ),
        Causal[Map[Int, DotSet]](
          Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        )
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )
  }

  test("Causal with DotMap should merge dotstore recursively") {
    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](
          Map(1 -> Set(dot(1, "A"))),
          Dots.from(Map("A" -> 1))
        ),
        Causal[Map[Int, DotSet]](
          Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        )
      ),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"), dot(1, "B"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  test("Causal with DotMap should respect CausalContext when merging dotstore recursively") {
    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](
          Map(1 -> Set(dot(1, "A"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        ),
        Causal[Map[Int, DotSet]](
          Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        )
      ),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )

    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](
          Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        ),
        Causal[Map[Int, DotSet]](
          Map(1 -> Set(dot(1, "A"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        )
      ),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  test("Causal with DotMap should remove entry when merging of value yields bottom") {
    assertEquals(
      Lattice.merge(
        Causal[Map[Int, DotSet]](
          Map(1 -> Set(dot(1, "A"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        ),
        Causal[Map[Int, DotSet]](
          Map(1 -> Set(dot(1, "B"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        )
      ),
      Causal[Map[Int, DotSet]](Map[Int, DotSet](), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }
}
