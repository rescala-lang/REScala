package kofre.encrdt.crdts
import kofre.base.Lattice
import kofre.time.{Dots, Dot}
import kofre.contextual.{AsCausalContext, WithContext}
import kofre.dotted.DotFun

object DeltaMultiValueRegister {
  type DeltaMultiValueRegisterLattice[V] = WithContext[DotFun[V]]

  def deltaWrite[V](
      value: V,
      replicaId: String,
      register: DeltaMultiValueRegisterLattice[V]
  ): DeltaMultiValueRegisterLattice[V] = {

    val dot = register.context.clockOf(replicaId).get.advance
    WithContext(
      DotFun(Map(dot -> value)),
      Dots.fromSet(register.store.keySet + dot)
      )
  }

  def deltaClear[V: Lattice](register: DeltaMultiValueRegisterLattice[V]): DeltaMultiValueRegisterLattice[V] =
    WithContext(
      DotFun.empty,
      Dots.fromSet(register.store.keySet)
      )

  def read[V](register: DeltaMultiValueRegisterLattice[V]): Set[V] = {
    register.store.values.toSet
  }
}
