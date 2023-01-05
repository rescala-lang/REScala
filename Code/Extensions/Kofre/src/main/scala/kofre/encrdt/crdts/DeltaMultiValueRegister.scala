package kofre.encrdt.crdts
import kofre.base.{Lattice, Id}
import kofre.time.{Dots, Dot}
import kofre.dotted.{DotFun, Dotted, HasDots}

object DeltaMultiValueRegister {
  type DeltaMultiValueRegisterLattice[V] = Dotted[DotFun[V]]

  def deltaWrite[V](
      value: V,
      replicaId: Id ,
      register: DeltaMultiValueRegisterLattice[V]
  ): DeltaMultiValueRegisterLattice[V] = {

    val dot = register.context.clockOf(replicaId).get.advance
    Dotted(
      DotFun(Map(dot -> value)),
      Dots.from(register.store.keySet + dot)
    )
  }

  def deltaClear[V: Lattice](register: DeltaMultiValueRegisterLattice[V]): DeltaMultiValueRegisterLattice[V] =
    Dotted(
      DotFun.empty,
      Dots.from(register.store.keySet)
    )

  def read[V](register: DeltaMultiValueRegisterLattice[V]): Set[V] = {
    register.store.values.toSet
  }
}
