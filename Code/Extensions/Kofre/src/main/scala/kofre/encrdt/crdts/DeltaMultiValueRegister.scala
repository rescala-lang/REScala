package kofre.encrdt.crdts
import kofre.Lattice
import kofre.causality.DotStore.{DotFun, dotFunDotStore}
import kofre.causality.{CausalContext, DotStore}
import kofre.encrdt.lattices.Causal

object DeltaMultiValueRegister {
  type DeltaMultiValueRegisterLattice[V] = Causal[DotFun[V]]

  def deltaWrite[V](
      value: V,
      replicaId: String,
      register: DeltaMultiValueRegisterLattice[V]
  ): DeltaMultiValueRegisterLattice[V] = {

    val dot = register.causalContext.clockOf(replicaId).get.advance
    Causal(
      Map(dot -> value),
      CausalContext.fromSet(register.dotStore.keySet + dot)
    )
  }

  def deltaClear[V: Lattice](register: DeltaMultiValueRegisterLattice[V]): DeltaMultiValueRegisterLattice[V] =
    Causal(
      DotStore[DotFun[V]].bottom,
      CausalContext.fromSet(register.dotStore.keySet)
    )

  def read[V](register: DeltaMultiValueRegisterLattice[V]): Set[V] = {
    register.dotStore.values.toSet
  }
}
