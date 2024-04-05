package com.github.ckuessner.encrdt.crdts.delta

import com.github.ckuessner.encrdt.causality.impl.ArrayCausalContext
import com.github.ckuessner.encrdt.causality.DotStore.{Dot, DotMap, DotSet}
import com.github.ckuessner.encrdt.crdts.DeltaAddWinsMap
import DeltaAddWinsMap.DeltaAddWinsMapLattice
import com.github.ckuessner.encrdt.causality.{CausalContext, LamportClock}
import com.github.ckuessner.encrdt.lattices.{Causal, SemiLattice}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import scala.language.implicitConversions

class DeltaAddWinsMapSpec extends AnyFlatSpec {
  def dot(time: Long, replicaId: String): LamportClock = LamportClock(time, replicaId)

  implicit def pairToDot(tuple: (String, Int)): Dot = dot(tuple._2, tuple._1)

  import scala.language.implicitConversions

  private implicit def setOfDotsToCausalContext(setOfDots: Set[LamportClock]): CausalContext = {
    CausalContext(ArrayCausalContext.fromSet(setOfDots))
  }

  private implicit def setOfDotsToDotSet(setOfDots: Set[LamportClock]): DotSet = {
    ArrayCausalContext.fromSet(setOfDots)
  }

  "Lattice" should "merge if empty" in {
    SemiLattice.merged(
      DeltaAddWinsMap.bottom[Int, DotSet],
      DeltaAddWinsMap.bottom[Int, DotSet],
    ) should ===(DeltaAddWinsMap.bottom[Int, DotSet])
  }

  it should "merge concurrent updates" in {
    SemiLattice[DeltaAddWinsMapLattice[Int, DotSet]].merged(
      Causal(Map(1 -> Set(dot(1, "A"))), Set(dot(1, "A"))),
      Causal(Map(1 -> Set(dot(1, "B"))), Set(dot(1, "B"))),
    ) should ===(Causal[DotMap[Int, DotSet]](
      Map(1 -> Set(dot(1, "A"), dot(1, "B"))),
      Set(dot(1, "A"), dot(1, "B"))
    ))
  }

}
