package kofre.encrdt

import kofre.base.{DecomposeLattice, Id, Lattice}
import kofre.datatypes.AddWinsSet
import kofre.dotted.Dotted
import kofre.syntax.PermIdMutate.withID
import kofre.syntax.{DottedName, PermIdMutate}
import kofre.time.Dots

case class AddWinsMapLattice[K, V](
    keys: Dotted[AddWinsSet[K]] = Dotted(AddWinsSet.empty[K]),
    mappings: Map[K, V] = Map[K, V]()
) {
  def values: Map[K, V] = mappings

  def added(key: K, value: V, replicaId: Id): AddWinsMapLattice[K, V] = {
    val newKeys = keys merge keys.named(replicaId).add(key).anon
    val newMap  = mappings + (key -> value)
    AddWinsMapLattice(newKeys, newMap)
  }

  def removed(key: K): AddWinsMapLattice[K, V] = {
    val newKeys = keys merge keys.remove(key)
    val newMap  = mappings - key
    AddWinsMapLattice(newKeys, newMap)
  }
}

object AddWinsMapLattice {
  given AddWinsLattice[K, V: Lattice]: Lattice[AddWinsMapLattice[K, V]] = Lattice.derived
}
