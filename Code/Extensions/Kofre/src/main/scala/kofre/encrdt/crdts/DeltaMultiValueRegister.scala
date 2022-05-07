package kofre.encrdt.crdts
import kofre.Lattice
import kofre.causality.CausalContext
import kofre.dotbased.{WithContext, DotStore}
import kofre.dotbased.DotStore.DotFun

object DeltaMultiValueRegister {
  type DeltaMultiValueRegisterLattice[V] = WithContext[DotFun[V]]

  def deltaWrite[V](
      value: V,
      replicaId: String,
      register: DeltaMultiValueRegisterLattice[V]
  ): DeltaMultiValueRegisterLattice[V] = {

    val dot = register.context.clockOf(replicaId).get.advance
    WithContext(
      Map(dot -> value),
      CausalContext.fromSet(register.store.keySet + dot)
    )
  }

  def deltaClear[V: Lattice](register: DeltaMultiValueRegisterLattice[V]): DeltaMultiValueRegisterLattice[V] =
    WithContext(
      Map.empty,
      CausalContext.fromSet(register.store.keySet)
      )

  def read[V](register: DeltaMultiValueRegisterLattice[V]): Set[V] = {
    register.store.values.toSet
  }
}
