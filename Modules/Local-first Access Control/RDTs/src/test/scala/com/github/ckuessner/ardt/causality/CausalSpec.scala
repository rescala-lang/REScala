package com.github.ckuessner.ardt.causality

import com.github.ckuessner.ardt.base.Causal.given
import com.github.ckuessner.ardt.base.StandardLibrary.GrowOnlySet.given
import com.github.ckuessner.ardt.base.{Causal, Lattice}
import com.github.ckuessner.ardt.causality.DotStore.{DotFun, DotSet}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import rdts.time.{Dot, Dots}

class CausalSpec extends AnyFlatSpec {
  def dot(time: Long, replicaId: String): Dot = Dot(replicaId, time)

  import scala.language.implicitConversions

  private given mapToSetOfDots: Conversion[Map[String, Int], Set[Dot]] =
    _.flatMap(tuple =>
      1 to tuple._2 map {
        Dot(tuple._1, _)
      }
    ).toSet

  private given setOfDotsToDotSet: Conversion[Set[Dot], DotSet] = Dots.from(_)

  "Causal with DotSet" should "merge when empty" in {
    Lattice[Causal[DotSet]].merge(
      Causal.bottom[DotSet],
      Causal.bottom[DotSet]
    )
  }

  it should "union the CausalContext" in {
    Lattice[Causal[DotSet]].merge(
      Causal(Set.empty[Dot], Dots.from(Map("A" -> 1))),
      Causal(Set.empty[Dot], Dots.from(Map("B" -> 2)))
    ) should ===(Causal[DotSet](Set.empty[Dot], Dots.from(Map("A" -> 1, "B" -> 2))))

    Lattice[Causal[DotSet]].merge(
      Causal(Set.empty[Dot], Dots.from(Map("A" -> 1, "B" -> 1))),
      Causal(Set.empty[Dot], Dots.from(Map("B" -> 2)))
    ) should ===(Causal[DotSet](Set.empty[Dot], Dots.from(Map("A" -> 1, "B" -> 2))))
  }

  it should "merge with disjoint dotset" in {
    Lattice[Causal[DotSet]].merge(
      Causal(Set(dot(1, "A")), Dots.from(Map("A" -> 1))),
      Causal(Set(dot(1, "B")), Dots.from(Map("B" -> 1)))
    ) should ===(
      Causal[DotSet](Set(dot(1, "A"), dot(1, "B")), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "remove dots from dotset that are contained in CausalContext" in {
    Lattice[Causal[DotSet]].merge(
      Causal(Set(dot(1, "A")), Dots.from(Map("A" -> 1))),
      Causal(Set(dot(2, "A")), Dots.from(Map("A" -> 2)))
    ) should ===(
      Causal[DotSet](
        Set(dot(2, "A")),
        Dots.from(Map("A" -> 2))
      )
    )
  }

  it should "remove dots from dotset if removed" in {
    Lattice[Causal[DotSet]].merge(
      Causal(Set(dot(2, "A")), Dots.from(Map("A" -> 2))),
      Causal(Set.empty[Dot], Dots.from(Map("A" -> 2)))
    ) should ===(
      Causal[DotSet](
        Set.empty[Dot],
        Dots.from(Map("A" -> 2))
      )
    )

    Lattice[Causal[DotSet]].merge(
      Causal(Set.empty[Dot], Dots.from(Map("A" -> 2))),
      Causal(Set(dot(2, "A")), Dots.from(Map("A" -> 2)))
    ) should ===(
      Causal[DotSet](
        Set.empty[Dot],
        Dots.from(Map("A" -> 2))
      )
    )

    Lattice[Causal[DotSet]].merge(
      Causal(Set(dot(1, "B")), Dots.from(Map("A" -> 2, "B" -> 1))),
      Causal(Set(dot(2, "A")), Dots.from(Map("A" -> 2)))
    ) should ===(
      Causal[DotSet](
        Set(dot(1, "B")),
        Dots.from(Map("A" -> 2, "B" -> 1))
      )
    )
  }

  "Causal with DotFun" should "merge when empty" in {
    Lattice[Causal[DotFun[Set[Int]]]].merge(
      Causal(Map(), Dots.empty),
      Causal(Map(), Dots.empty)
    ) should ===(
      Causal[DotFun[Set[Int]]](Map(), Dots.empty)
    )
  }

  it should "merge idempotent" in {
    Lattice[Causal[DotFun[Set[Int]]]].merge(
      Causal(Map(dot(1, "A") -> Set(1)), Dots.from(Map("A" -> 1))),
      Causal(Map(dot(1, "A") -> Set(1)), Dots.from(Map("A" -> 1)))
    ) should ===(
      Causal(Map(dot(1, "A") -> Set(1)), Dots.from(Map("A" -> 1)))
    )

    Lattice[Causal[DotFun[Set[Int]]]].merge(
      Causal(Map(), Dots.from(Map("A" -> 2))),
      Causal(Map(), Dots.from(Map("A" -> 2)))
    ) should ===(
      Causal(Map[Dot, Set[Int]](), Dots.from(Map("A" -> 2)))
    )

    Lattice[Causal[DotFun[Set[Int]]]].merge(
      Causal(Map(dot(1, "A") -> Set()), Dots.from(Map("A" -> 2))),
      Causal(Map(dot(1, "A") -> Set()), Dots.from(Map("A" -> 2)))
    ) should ===(
      Causal(Map(dot(1, "A") -> Set()), Dots.from(Map("A" -> 2)))
    )
  }

  it should "merge lattice when dot contained in both dotstores" in {
    Lattice[Causal[DotFun[Set[Int]]]].merge(
      Causal(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1))),
      Causal(Map(dot(1, "A") -> Set(21)), Dots.from(Map("A" -> 1)))
    ) should ===(
      Causal(
        Map(
          dot(1, "A") -> Set(21, 42)
        ),
        Dots.from(Map("A" -> 1))
      )
    )
  }

  it should "union dotstore when disjoint dotstores and not a causal removal" in {
    Lattice[Causal[DotFun[Set[Int]]]].merge(
      Causal(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1))),
      Causal(Map(dot(1, "B") -> Set(21)), Dots.from(Map("B" -> 1)))
    ) should ===(
      Causal(
        Map(
          dot(1, "A") -> Set(42),
          dot(1, "B") -> Set(21)
        ),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )
  }

  it should "discard values in dotstore when not contained in causal context and has disjoint dotstores" in {
    Lattice[Causal[DotFun[Set[Int]]]].merge(
      Causal(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1))),
      Causal(Map(dot(1, "B") -> Set(21)), Dots.from(Map("A" -> 1, "B" -> 1)))
    ) should ===(
      Causal(
        Map(
          dot(1, "B") -> Set(21)
        ),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )

    Lattice[Causal[DotFun[Set[Int]]]].merge(
      Causal(Map(dot(1, "B") -> Set(21)), Dots.from(Map("A" -> 1, "B" -> 1))),
      Causal(Map(dot(1, "A") -> Set(42)), Dots.from(Map("A" -> 1)))
    ) should ===(
      Causal(
        Map(
          dot(1, "B") -> Set(21)
        ),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )
  }

  "Causal with DotMap" should "merge when empty" in {
    Lattice.merge(
      Causal[Map[Int, DotSet]](Map(), Dots.empty),
      Causal[Map[Int, DotSet]](Map(), Dots.empty)
    ) should ===(Causal[Map[Int, DotSet]](Map(), Dots.empty))
  }

  it should "merge idempotent" in {
    Lattice.merge(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1)))
    ) should ===(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1)))
    )
  }

  it should "remove values when removal is causal" in {
    Lattice.merge(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
      Causal[Map[Int, DotSet]](Map(), Dots.from(Map("A" -> 2)))
    ) should ===(
      Causal[Map[Int, DotSet]](Map(), Dots.from(Map("A" -> 2)))
    )

    Lattice.merge(
      Causal[Map[Int, DotSet]](Map(), Dots.from(Map("A" -> 2))),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1)))
    ) should ===(
      Causal[Map[Int, DotSet]](Map(), Dots.from(Map("A" -> 2)))
    )
  }

  it should "replace dotstore when causal" in {
    Lattice.merge(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "B"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    ) should ===(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "B"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "merge dotstore when disjoint" in {
    Lattice.merge(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1))),
      Causal[Map[Int, DotSet]](Map(2 -> Set(dot(1, "B"))), Dots.from(Map("B" -> 1)))
    ) should ===(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )
  }

  it should "merge dotstore when not disjoint" in {
    Lattice.merge(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        Dots.from(Map("A" -> 1))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    ) should ===(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    )
  }

  it should "merge dotstore recursively" in {
    Lattice.merge(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        Dots.from(Map("A" -> 1))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    ) should ===(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"), dot(1, "B"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "respect CausalContext when merging dotstore recursively" in {
    Lattice.merge(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    ) should ===(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )

    Lattice.merge(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    ) should ===(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "remove entry when merging of value yields bottom" in {
    Lattice.merge(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "B"))),
        Dots.from(Map("A" -> 1, "B" -> 1))
      )
    ) should ===(
      Causal[Map[Int, DotSet]](Map[Int, DotSet](), Dots.from(Map("A" -> 1, "B" -> 1)))
    )
  }
}
