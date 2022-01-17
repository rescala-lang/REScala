
package kofre.encrdt.crdts
import kofre.Lattice

import kofre.encrdt.causality.DotStore.DotFun
import kofre.encrdt.causality.LamportClock
import kofre.encrdt.crdts.DeltaAddWinsLastWriterWinsMap.{DeltaAddWinsLastWriterWinsMapLattice, timestampedValueLattice}
import kofre.encrdt.crdts.DeltaAddWinsMap.DeltaAddWinsMapLattice
import kofre.encrdt.crdts.interfaces.MapCrdt
import kofre.encrdt.lattices.LastWriterWinsTagLattice.lwwLattice

import java.time.Instant

class DeltaAddWinsLastWriterWinsMap[K, V](val replicaId: String,
                                          initialState: DeltaAddWinsLastWriterWinsMapLattice[K, V] = DeltaAddWinsLastWriterWinsMap.bottom[K, V],
                                          initialDeltas: Vector[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = Vector()
                                         ) extends MapCrdt[K, V] {
  var _state: DeltaAddWinsLastWriterWinsMapLattice[K, V] = initialState
  var _deltas: Vector[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = initialDeltas

  def state: DeltaAddWinsLastWriterWinsMapLattice[K, V] = _state

  def deltas: Vector[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = _deltas

  override def get(key: K): Option[V] =
    _state.dotStore
      .getOrElse(key, Set.empty[(_, (V, (Instant, String)))])
      .map(_._2)
      .maxByOption(_._2)
      .map(_._1)

  override def put(key: K, value: V): Unit =
    mutate(
      DeltaAddWinsMap.deltaMutate(
        key,
        DeltaMultiValueRegister.deltaWrite((value, (Instant.now(), replicaId)), replicaId, _),
        _state
      )
    )

  def putDelta(key: K, value: V): DeltaAddWinsMapLattice[K, DotFun[(V, (Instant, String))]] = {
    val delta: DeltaAddWinsMapLattice[K, DotFun[(V, (Instant, String))]] =
      DeltaAddWinsMap.deltaMutate(
        key,
        DeltaMultiValueRegister.deltaWrite((value, (Instant.now(), replicaId)), replicaId, _),
        _state
      )
    mutate(delta)
    delta
  }

  override def remove(key: K): Unit =
    mutate(DeltaAddWinsMap.deltaRemove(key, _state))

  override def values: Map[K, V] =
    _state.dotStore.map { case (k, mvReg) =>
      k -> mvReg.values.maxBy(_._2)._1
    }

  def merge(other: DeltaAddWinsLastWriterWinsMapLattice[K, V]): Unit = {
    mutate(other)
  }

  private def mutate(delta: DeltaAddWinsLastWriterWinsMapLattice[K, V]): Unit = {
    _deltas = _deltas.appended(delta)
    _state = Lattice[DeltaAddWinsLastWriterWinsMapLattice[K, V]].merge(_state, delta)
  }
}

object DeltaAddWinsLastWriterWinsMap {
  type DeltaAddWinsLastWriterWinsMapLattice[K, V] =
    DeltaAddWinsMapLattice[K, DotFun[(V, (Instant, String))]]

  def bottom[K, V]: DeltaAddWinsLastWriterWinsMapLattice[K, V] = DeltaAddWinsMap.bottom[K, DotFun[(V, (Instant, String))]]

  implicit def timestampedValueLattice[V]: Lattice[(V, (Instant, String))] = (left, right) => {
    if (Lattice.merge(left._2, right._2) == left._2) left
    else right
  }

  type StateType[K, V] = DeltaAddWinsMapLattice[K, DotFun[(V, (Instant, String))]]
}
