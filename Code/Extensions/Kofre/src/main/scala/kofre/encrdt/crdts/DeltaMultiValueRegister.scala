
package kofre.encrdt.crdts
import kofre.encrdt.causality.DotStore
import kofre.encrdt.causality.DotStore.{DotFun, dotFunDotStore}
import kofre.encrdt.lattices.{Causal}
import kofre.Lattice

object DeltaMultiValueRegister {
  type DeltaMultiValueRegisterLattice[V] = Causal[DotFun[V]]

  def deltaWrite[V](value: V,
                    replicaId: String,
                    register: DeltaMultiValueRegisterLattice[V]
                   ): DeltaMultiValueRegisterLattice[V] = {

    val dot = register.causalContext.clockOf(replicaId).advance(replicaId)
    Causal(
      Map(dot -> value),
      register.dotStore.keySet + dot
    )
  }

  def deltaClear[V: Lattice](register: DeltaMultiValueRegisterLattice[V]): DeltaMultiValueRegisterLattice[V] =
    Causal(
      DotStore[DotFun[V]].bottom,
      register.dotStore.keySet
    )

  def read[V](register: DeltaMultiValueRegisterLattice[V]): Set[V] = {
    register.dotStore.values.toSet
  }
}
