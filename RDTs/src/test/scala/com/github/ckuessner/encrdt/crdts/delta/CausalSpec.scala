package com.github.ckuessner.encrdt.crdts.delta

import com.github.ckuessner.encrdt.causality.{CausalContext, LamportClock}
import com.github.ckuessner.encrdt.causality.impl.ArrayCausalContext
import com.github.ckuessner.encrdt.causality.DotStore.{Dot, DotFun, DotSet}
import com.github.ckuessner.encrdt.lattices.{Causal, SemiLattice}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper

class CausalSpec extends AnyFlatSpec {
  def dot(time: Long, replicaId: String): LamportClock = LamportClock(time, replicaId)

  import scala.language.implicitConversions

  private implicit def mapToSetOfDots(map: Map[String, Int]): Set[Dot] =
    map.flatMap(tuple =>
      1 to tuple._2 map {
        LamportClock(_, tuple._1)
      }
    ).toSet

  private implicit def setOfDotsToDotSet(setOfDots: Set[Dot]): DotSet = {
    ArrayCausalContext.fromSet(setOfDots)
  }

  "Causal with DotSet" should "merge when empty" in {
    SemiLattice[Causal[DotSet]].merged(
      Causal.bottom[DotSet],
      Causal.bottom[DotSet]
    )
  }

  it should "union the CausalContext" in {
    SemiLattice[Causal[DotSet]].merged(
      Causal(Set.empty[Dot], CausalContext(Map("A" -> 1))),
      Causal(Set.empty[Dot], CausalContext(Map("B" -> 2)))
    ) should ===(Causal[DotSet](Set.empty[Dot], CausalContext(Map("A" -> 1, "B" -> 2))))

    SemiLattice[Causal[DotSet]].merged(
      Causal(Set.empty[Dot], CausalContext(Map("A" -> 1, "B" -> 1))),
      Causal(Set.empty[Dot], CausalContext(Map("B" -> 2)))
    ) should ===(Causal[DotSet](Set.empty[Dot], CausalContext(Map("A" -> 1, "B" -> 2))))
  }

  it should "merge with disjoint dotset" in {
    SemiLattice[Causal[DotSet]].merged(
      Causal(Set(dot(1, "A")), CausalContext(Map("A" -> 1))),
      Causal(Set(dot(1, "B")), CausalContext(Map("B" -> 1)))
    ) should ===(
      Causal[DotSet](
        Set(dot(1, "A"), dot(1, "B")),
        CausalContext(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "remove dots from dotset that are contained in CausalContext" in {
    SemiLattice[Causal[DotSet]].merged(
      Causal(Set(dot(1, "A")), CausalContext(Map("A" -> 1))),
      Causal(Set(dot(2, "A")), CausalContext(Map("A" -> 2)))
    ) should ===(
      Causal[DotSet](
        Set(dot(2, "A")),
        CausalContext(Map("A" -> 2))
      )
    )
  }

  it should "remove dots from dotset if removed" in {
    SemiLattice[Causal[DotSet]].merged(
      Causal(Set(dot(2, "A")), CausalContext(Map("A" -> 2))),
      Causal(Set.empty[Dot], CausalContext(Map("A" -> 2)))
    ) should ===(
      Causal[DotSet](
        Set.empty[Dot],
        CausalContext(Map("A" -> 2))
      )
    )

    SemiLattice[Causal[DotSet]].merged(
      Causal(Set.empty[Dot], CausalContext(Map("A" -> 2))),
      Causal(Set(dot(2, "A")), CausalContext(Map("A" -> 2))),
    ) should ===(
      Causal[DotSet](
        Set.empty[Dot],
        CausalContext(Map("A" -> 2))
      )
    )

    SemiLattice[Causal[DotSet]].merged(
      Causal(Set(dot(1, "B")), CausalContext(Map("A" -> 2, "B" -> 1))),
      Causal(Set(dot(2, "A")), CausalContext(Map("A" -> 2))),
    ) should ===(
      Causal[DotSet](
        Set(dot(1, "B")),
        CausalContext(Map("A" -> 2, "B" -> 1))
      )
    )
  }

  "Causal with DotFun" should "merge when empty" in {
    import com.github.ckuessner.encrdt.lattices.GrowOnlySetLattice.SetLattice
    SemiLattice[Causal[DotFun[Set[Int]]]].merged(
      Causal(Map(), CausalContext()),
      Causal(Map(), CausalContext())
    ) should ===(
      Causal[DotFun[Set[Int]]](Map(), CausalContext())
    )
  }

  it should "merge idempotent" in {
    import com.github.ckuessner.encrdt.lattices.GrowOnlySetLattice.SetLattice
    SemiLattice[Causal[DotFun[Set[Int]]]].merged(
      Causal(Map(dot(1, "A") -> Set(1)), CausalContext(Map("A" -> 1))),
      Causal(Map(dot(1, "A") -> Set(1)), CausalContext(Map("A" -> 1)))
    ) should ===(
      Causal(Map(dot(1, "A") -> Set(1)), CausalContext(Map("A" -> 1)))
    )

    SemiLattice[Causal[DotFun[Set[Int]]]].merged(
      Causal(Map(), CausalContext(Map("A" -> 2))),
      Causal(Map(), CausalContext(Map("A" -> 2)))
    ) should ===(
      Causal(Map[Dot, Set[Int]](), CausalContext(Map("A" -> 2)))
    )

    SemiLattice[Causal[DotFun[Set[Int]]]].merged(
      Causal(Map(dot(1, "A") -> Set()), CausalContext(Map("A" -> 2))),
      Causal(Map(dot(1, "A") -> Set()), CausalContext(Map("A" -> 2)))
    ) should ===(
      Causal(Map(dot(1, "A") -> Set()), CausalContext(Map("A" -> 2)))
    )
  }

  it should "merge lattice when dot contained in both dotstores" in {
    import com.github.ckuessner.encrdt.lattices.GrowOnlySetLattice.SetLattice
    SemiLattice[Causal[DotFun[Set[Int]]]].merged(
      Causal(Map(dot(1, "A") -> Set(42)), CausalContext(Map("A" -> 1))),
      Causal(Map(dot(1, "A") -> Set(21)), CausalContext(Map("A" -> 1)))
    ) should ===(
      Causal(Map(
        dot(1, "A") -> Set(21, 42),
      ), CausalContext(Map("A" -> 1)))
    )
  }

  it should "union dotstore when disjoint dotstores and not a causal removal" in {
    import com.github.ckuessner.encrdt.lattices.GrowOnlySetLattice.SetLattice
    SemiLattice[Causal[DotFun[Set[Int]]]].merged(
      Causal(Map(dot(1, "A") -> Set(42)), CausalContext(Map("A" -> 1))),
      Causal(Map(dot(1, "B") -> Set(21)), CausalContext(Map("B" -> 1)))
    ) should ===(
      Causal(Map(
        dot(1, "A") -> Set(42),
        dot(1, "B") -> Set(21),
      ), CausalContext(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "discard values in dotstore when not contained in causal context and has disjoint dotstores" in {
    import com.github.ckuessner.encrdt.lattices.GrowOnlySetLattice.SetLattice
    SemiLattice[Causal[DotFun[Set[Int]]]].merged(
      Causal(Map(dot(1, "A") -> Set(42)), CausalContext(Map("A" -> 1))),
      Causal(Map(dot(1, "B") -> Set(21)), CausalContext(Map("A" -> 1, "B" -> 1)))
    ) should ===(
      Causal(Map(
        dot(1, "B") -> Set(21),
      ), CausalContext(Map("A" -> 1, "B" -> 1)))
    )

    SemiLattice[Causal[DotFun[Set[Int]]]].merged(
      Causal(Map(dot(1, "B") -> Set(21)), CausalContext(Map("A" -> 1, "B" -> 1))),
      Causal(Map(dot(1, "A") -> Set(42)), CausalContext(Map("A" -> 1)))
    ) should ===(
      Causal(Map(
        dot(1, "B") -> Set(21),
      ), CausalContext(Map("A" -> 1, "B" -> 1)))
    )
  }

  "Causal with DotMap" should "merge when empty" in {
    SemiLattice.merged(
      Causal[Map[Int, DotSet]](Map(), CausalContext()),
      Causal[Map[Int, DotSet]](Map(), CausalContext())
    ) should ===(Causal[Map[Int, DotSet]](Map(), CausalContext()))
  }

  it should "merge idempotent" in {
    SemiLattice.merged(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), CausalContext(Map("A" -> 1))),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), CausalContext(Map("A" -> 1)))
    ) should ===(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), CausalContext(Map("A" -> 1)))
    )
  }

  it should "remove values when removal is causal" in {
    SemiLattice.merged(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), CausalContext(Map("A" -> 1))),
      Causal[Map[Int, DotSet]](Map(), CausalContext(Map("A" -> 2)))
    ) should ===(
      Causal[Map[Int, DotSet]](Map(), CausalContext(Map("A" -> 2)))
    )

    SemiLattice.merged(
      Causal[Map[Int, DotSet]](Map(), CausalContext(Map("A" -> 2))),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), CausalContext(Map("A" -> 1)))
    ) should ===(
      Causal[Map[Int, DotSet]](Map(), CausalContext(Map("A" -> 2)))
    )
  }

  it should "replace dotstore when causal" in {
    SemiLattice.merged(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), CausalContext(Map("A" -> 1))),
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "B"))), CausalContext(Map("A" -> 1, "B" -> 1)))
    ) should ===(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "B"))), CausalContext(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "merge dotstore when disjoint" in {
    SemiLattice.merged(
      Causal[Map[Int, DotSet]](Map(1 -> Set(dot(1, "A"))), CausalContext(Map("A" -> 1))),
      Causal[Map[Int, DotSet]](Map(2 -> Set(dot(1, "B"))), CausalContext(Map("B" -> 1)))
    ) should ===(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
        CausalContext(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "merge dotstore when not disjoint" in {
    SemiLattice.merged(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        CausalContext(Map("A" -> 1))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
        CausalContext(Map("A" -> 1, "B" -> 1))
      )
    ) should ===(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A")), 2 -> Set(dot(1, "B"))),
        CausalContext(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "merge dotstore recursively" in {
    SemiLattice.merged(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        CausalContext(Map("A" -> 1))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
        CausalContext(Map("A" -> 1, "B" -> 1))
      )
    ) should ===(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
        CausalContext(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "respect CausalContext when merging dotstore recursively" in {
    SemiLattice.merged(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        CausalContext(Map("A" -> 1, "B" -> 1))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
        CausalContext(Map("A" -> 1, "B" -> 1))
      )
    ) should ===(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        CausalContext(Map("A" -> 1, "B" -> 1)))
    )

    SemiLattice.merged(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
        CausalContext(Map("A" -> 1, "B" -> 1))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        CausalContext(Map("A" -> 1, "B" -> 1))
      )
    ) should ===(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        CausalContext(Map("A" -> 1, "B" -> 1)))
    )
  }

  it should "remove entry when merging of value yields bottom" in {
    SemiLattice.merged(
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"))),
        CausalContext(Map("A" -> 1, "B" -> 1))
      ),
      Causal[Map[Int, DotSet]](
        Map(1 -> Set(dot(1, "B"))),
        CausalContext(Map("A" -> 1, "B" -> 1))
      )
    ) should ===(
      Causal[Map[Int, DotSet]](
        Map[Int, DotSet](),
        CausalContext(Map("A" -> 1, "B" -> 1)))
    )
  }
}
