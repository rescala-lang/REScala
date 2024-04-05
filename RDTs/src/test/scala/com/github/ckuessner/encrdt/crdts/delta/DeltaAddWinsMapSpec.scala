package com.github.ckuessner.encrdt.crdts.delta

import com.github.ckuessner.encrdt.causality.CausalContext.*
import com.github.ckuessner.encrdt.causality.DotStore.{DotMap, DotSet}
import com.github.ckuessner.encrdt.causality.impl.ArrayCausalContext
import com.github.ckuessner.encrdt.causality.{CausalContext, Dot}
import com.github.ckuessner.encrdt.crdts.DeltaAddWinsMap
import com.github.ckuessner.encrdt.crdts.DeltaAddWinsMap.DeltaAddWinsMapLattice
import com.github.ckuessner.encrdt.lattices.{Causal, SemiLattice}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*

import scala.language.implicitConversions

class DeltaAddWinsMapSpec extends AnyFlatSpec {
  def dot(time: Long, replicaId: String): Dot = Dot(time, replicaId)

  given pairToDot: Conversion[(String, Int), Dot] = { case (s, i) => dot(i, s) }

  given convSetOfDotsToCausalContext: Conversion[Set[Dot], CausalContext] = dotSet => {
    CausalContext(ArrayCausalContext.fromSet(dotSet))
  }

  given setOfDotsToDotSet: Conversion[Set[Dot], DotSet] = ArrayCausalContext.fromSet(_)

  "Lattice" should "merge if empty" in {
    SemiLattice.merged(
      DeltaAddWinsMap.bottom[Int, DotSet],
      DeltaAddWinsMap.bottom[Int, DotSet]
    ) should ===(DeltaAddWinsMap.bottom[Int, DotSet])
  }

  it should "merge concurrent updates" in {
    SemiLattice[DeltaAddWinsMapLattice[Int, DotSet]].merged(
      Causal(Map(1 -> Set(dot(1, "A"))), Set(dot(1, "A"))),
      Causal(Map(1 -> Set(dot(1, "B"))), Set(dot(1, "B")))
    ) should ===(
      Causal[DotMap[Int, DotSet]](
        Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
        Set(dot(1, "A"), dot(1, "B"))
      )
    )
  }

}
