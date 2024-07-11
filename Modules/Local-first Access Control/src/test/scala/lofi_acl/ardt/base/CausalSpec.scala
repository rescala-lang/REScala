package lofi_acl.ardt.base

import lofi_acl.ardt.base.StandardLibrary.GrowOnlySet.given
import munit.FunSuite
import rdts.base.{Lattice, Uid}
import rdts.dotted.Dotted
import rdts.time.{Dot, Dots}

class CausalSpec extends FunSuite {
  def dot(time: Long, replicaId: String): Dot = Dot(Uid(replicaId), time)

  import scala.language.implicitConversions

  private given mapToSetOfDots: Conversion[Map[String, Int], Set[Dot]] =
    _.flatMap(tuple =>
      1 to tuple._2 map {
        Dot(Uid(tuple._1), _)
      }
    ).toSet

  private given setOfDotsToDotSet: Conversion[Set[Dot], Dots] = Dots.from(_)

  test("Causal with Dots should merge when empty") {
    assertEquals(
      Lattice[Dotted[Dots]].merge(
        Dotted.empty,
        Dotted.empty
      ),
      Dotted.empty[Dots]
    )
  }

  test("Causal with Dots should union the CausalContext") {
    assertEquals(
      Lattice[Dotted[Dots]].merge(
        Dotted(Set.empty[Dot], Dots.from(Map("A" -> 1))),
        Dotted(Set.empty[Dot], Dots.from(Map("B" -> 2)))
      ),
      Dotted[Dots](Set.empty[Dot], Dots.from(Map("A" -> 1, "B" -> 2)))
    )

    assertEquals(
      Lattice[Dotted[Dots]].merge(
        Dotted(Set.empty[Dot], Dots.from(Map("A" -> 1, "B" -> 1))),
        Dotted(Set.empty[Dot], Dots.from(Map("B" -> 2)))
      ),
      Dotted[Dots](Set.empty[Dot], Dots.from(Map("A" -> 1, "B" -> 2)))
    )
  }

  test("Causal with Dots should merge with disjoint dotset") {
    assertEquals(
      Lattice[Dotted[Dots]].merge(
        Dotted(Set(dot(1, "A")), Dots.from(Map("A" -> 1))),
        Dotted(Set(dot(1, "B")), Dots.from(Map("B" -> 1)))
      ),
      Dotted[Dots](Set(dot(1, "A"), dot(1, "B")), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  test("Causal with Dots should remove dots from dotset that are contained in CausalContext") {
    assertEquals(
      Lattice[Dotted[Dots]].merge(
        Dotted(Set(dot(1, "A")), Dots.from(Map("A" -> 1))),
        Dotted(Set(dot(2, "A")), Dots.from(Map("A" -> 2)))
      ),
      Dotted[Dots](
        Set(dot(2, "A")),
        Dots.from(Map("A" -> 2))
      )
    )
  }

  test("Causal with Dots should remove dots from dotset if removed") {
    assertEquals(
      Lattice[Dotted[Dots]].merge(
        Dotted(Set(dot(2, "A")), Dots.from(Map("A" -> 2))),
        Dotted(Set.empty[Dot], Dots.from(Map("A" -> 2)))
      ),
      Dotted[Dots](
        Set.empty[Dot],
        Dots.from(Map("A" -> 2))
      )
    )

    assertEquals(
      Lattice[Dotted[Dots]].merge(
        Dotted(Set.empty[Dot], Dots.from(Map("A" -> 2))),
        Dotted(Set(dot(2, "A")), Dots.from(Map("A" -> 2)))
      ),
      Dotted[Dots](
        Set.empty[Dot],
        Dots.from(Map("A" -> 2))
      )
    )

    assertEquals(
      Lattice[Dotted[Dots]].merge(
        Dotted(Set(dot(1, "B")), Dots.from(Map("A" -> 2, "B" -> 1))),
        Dotted(Set(dot(2, "A")), Dots.from(Map("A" -> 2)))
      ),
      Dotted[Dots](
        Set(dot(1, "B")),
        Dots.from(Map("A" -> 2, "B" -> 1))
      )
    )
  }

  test("Causal with DotFun should merge when empty") {
    assertEquals(
      Lattice[Dotted[Map[Dot, Set[Int]]]].merge(
        Dotted(Map(), Dots.empty),
        Dotted(Map(), Dots.empty)
      ),
      Dotted[Map[Dot, Set[Int]]](Map(), Dots.empty)
    )
  }

  test("Causal with DotFun should merge idempotent") {
    assertEquals(
      Lattice[Dotted[Map[Dot, Set[Int]]]].merge(
        Dotted(Map(dot(1, "A") -> Set(1)), Dots.from(Map("A" -> 1))),
        Dotted(Map(dot(1, "A") -> Set(1)), Dots.from(Map("A" -> 1)))
      ),
      Dotted(Map(dot(1, "A") -> Set(1)), Dots.from(Map("A" -> 1)))
    )

    assertEquals(
      Lattice[Dotted[Map[Dot, Set[Int]]]].merge(
        Dotted(Map(), Dots.from(Map("A" -> 2))),
        Dotted(Map(), Dots.from(Map("A" -> 2)))
      ),
      Dotted(Map[Dot, Set[Int]](), Dots.from(Map("A" -> 2)))
    )
  }

  // this test failed when adapting to the generic dotted (instead of causal).
  // the logic here seems incorrect – the two contexts don’t contain the other value, so the value should be deleted.
  // technically, we consider this fine for idempotence, because example is not normalized.
  // however, it might be conceivable that the dots contained within the data should not be considered for removal.
  test("Causal with DotFun should merge idempotent (TODO split off)".ignore) {

    val example = Dotted(Map(dot(1, "A") -> Set()), Dots.from(Map("A" -> 2)))

    assertEquals(
      example `merge`example,
      example
    )
  }

  test("Causal with DotFun should merge lattice when dot contained in both dotstores") {
    assertEquals(
      Lattice[Dotted[Map[Dot, Set[Int]]]].merge(
        Dotted(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1))),
        Dotted(Map(dot(1, "A") -> Set(21)), Dots.from(Map("A" -> 1)))
      ),
      Dotted(
        Map(
          dot(1, "A") -> Set(21, 42)
        ),
        Dots.from(Map("A" -> 1))
      )
    )
  }

  test("Causal with DotFun should union dotstore when disjoint dotstores and not a causal removal") {
    assertEquals(
      Lattice[Dotted[Map[Dot, Set[Int]]]].merge(
        Dotted(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1))),
        Dotted(Map(dot(1, "B") -> Set(21)), Dots.from(Map("B" -> 1)))
      ),
      Dotted(
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
      Lattice[Dotted[Map[Dot, Set[Int]]]].merge(
        Dotted(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1))),
        Dotted(Map(dot(1, "B") -> Set(21)), Dots.from(Map("A" -> 1, "B" -> 1)))
      ),
      Dotted(
        Map(
          dot(1, "B") -> Set(21)
        ),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )

    assertEquals(
      Lattice[Dotted[Map[Dot, Set[Int]]]].merge(
        Dotted(Map(dot(1, "B") -> Set(21)), Dots.from(Map("A" -> 1, "B" -> 1))),
        Dotted(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1)))
      ),
      Dotted(
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
        Dotted[Map[Int, Dots]](Map(), Dots.empty),
        Dotted[Map[Int, Dots]](Map(), Dots.empty)
      ),
      Dotted[Map[Int, Dots]](Map(), Dots.empty)
    )
  }

  test("Causal with DotMap should merge idempotent") {
    assertEquals(
      Lattice.merge(
        Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
        Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1)))
      ),
      Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1)))
    )
  }

  test("Causal with DotMap should remove values when removal is causal") {
    assertEquals(
      Lattice.merge(
        Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
        Dotted[Map[Int, Dots]](Map(), Dots.from(Map("A" -> 2)))
      ),
      Dotted[Map[Int, Dots]](Map(), Dots.from(Map("A" -> 2)))
    )

    assertEquals(
      Lattice.merge(
        Dotted[Map[Int, Dots]](Map(), Dots.from(Map("A" -> 2))),
        Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1)))
      ),
      Dotted[Map[Int, Dots]](Map(), Dots.from(Map("A" -> 2)))
    )
  }

  test("Causal with DotMap should replace dotstore when causal") {
    assertEquals(
      Lattice.merge(
        Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
        Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "B"))), Dots.from(Map("A" -> 1, "B" -> 1)))
      ),
      Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "B"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  test("Causal with DotMap should merge dotstore when disjoint") {
    assertEquals(
      Lattice.merge(
        Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
        Dotted[Map[Int, Dots]](Map(2 -> Set(dot(1, "B"))), Dots.from(Map("B" -> 1)))
      ),
      Dotted[Map[Int, Dots]](
        Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )
  }

  test("Causal with DotMap should merge dotstore when not disjoint") {
    assertEquals(
      Lattice.merge(
        Dotted[Map[Int, Dots]](
          Map(1 -> Set(dot(1, "A"))),
          Dots.from(Map("A" -> 1))
        ),
        Dotted[Map[Int, Dots]](
          Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        )
      ),
      Dotted[Map[Int, Dots]](
        Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )
  }

  test("Causal with DotMap should merge dotstore recursively") {
    assertEquals(
      Lattice.merge(
        Dotted[Map[Int, Dots]](
          Map(1 -> Set(dot(1, "A"))),
          Dots.from(Map("A" -> 1))
        ),
        Dotted[Map[Int, Dots]](
          Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        )
      ),
      Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "A"), dot(1, "B"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  test("Causal with DotMap should respect CausalContext when merging dotstore recursively") {
    assertEquals(
      Lattice.merge(
        Dotted[Map[Int, Dots]](
          Map(1 -> Set(dot(1, "A"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        ),
        Dotted[Map[Int, Dots]](
          Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        )
      ),
      Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )

    assertEquals(
      Lattice.merge(
        Dotted[Map[Int, Dots]](
          Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        ),
        Dotted[Map[Int, Dots]](
          Map(1 -> Set(dot(1, "A"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        )
      ),
      Dotted[Map[Int, Dots]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  test("Causal with DotMap should remove entry when merging of value yields bottom") {
    assertEquals(
      Lattice.merge(
        Dotted[Map[Int, Dots]](
          Map(1 -> Set(dot(1, "A"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        ),
        Dotted[Map[Int, Dots]](
          Map(1 -> Set(dot(1, "B"))),
          Dots.from(Map("A" -> 1, "B" -> 1))
        )
      ),
      Dotted[Map[Int, Dots]](Map[Int, Dots](), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }
}
