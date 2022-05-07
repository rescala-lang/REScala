package kofre.encrdt.crdts

import de.ckuessner.encrdt.crdts.interfaces.{Crdt, MapCrdt}
import kofre.encrdt.crdts.DeltaAddWinsLastWriterWinsMap.{DeltaAddWinsLastWriterWinsMapLattice, timestampedValueLattice}
import kofre.encrdt.crdts.DeltaAddWinsMap.DeltaAddWinsMapLattice
import kofre.Lattice
import kofre.contextual.WithContext
import kofre.causality.Dot
import kofre.encrdt.lattices.LastWriterWins

import math.Ordering.Implicits.infixOrderingOps
import java.time.Instant
import scala.collection.mutable.ArrayBuffer

class DeltaAddWinsLastWriterWinsMap[K, V](
    val replicaId: String,
    initialState: DeltaAddWinsLastWriterWinsMapLattice[K, V] = DeltaAddWinsLastWriterWinsMap.empty[K, V],
    initialDeltas: Vector[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = Vector()
) extends MapCrdt[K, V] with Crdt[DeltaAddWinsLastWriterWinsMapLattice[K, V]] {
  protected var _state: DeltaAddWinsLastWriterWinsMapLattice[K, V]               = initialState
  protected val _deltas: ArrayBuffer[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = ArrayBuffer.from(initialDeltas)

  def state: DeltaAddWinsLastWriterWinsMapLattice[K, V] = _state

  def deltas: Vector[DeltaAddWinsLastWriterWinsMapLattice[K, V]] =
    _deltas.toVector // TODO: Maybe change this to another type

  def get(key: K): Option[V] =
    _state.store
      .getOrElse(key, Set.empty[(_, (V, LastWriterWins[Instant, String]))])
      .map(_._2)
      .maxByOption(_._2)
      .map(_._1)

  def put(key: K, value: V): Unit =
    mutate(
      DeltaAddWinsMap.deltaMutate(
        key,
        DeltaMultiValueRegister.deltaWrite((value, LastWriterWins(Instant.now(), replicaId)), replicaId, _),
        _state
      )
    )

  def putDelta(key: K, value: V): DeltaAddWinsMapLattice[K, Map[Dot, (V, LastWriterWins[Instant, String])]] = {
    val delta: DeltaAddWinsMapLattice[K, Map[Dot, (V, LastWriterWins[Instant, String])]] =
      DeltaAddWinsMap.deltaMutate(
        key,
        DeltaMultiValueRegister.deltaWrite((value, LastWriterWins(Instant.now(), replicaId)), replicaId, _),
        _state
      )
    mutate(delta)
    delta
  }

  def remove(key: K): Unit =
    mutate(DeltaAddWinsMap.deltaRemove(key, _state))

  def removeDelta(key: K): DeltaAddWinsMapLattice[K, Map[Dot, (V, LastWriterWins[Instant, String])]] = {
    val delta = DeltaAddWinsMap.deltaRemove(key, _state)
    mutate(delta)
    delta
  }

  def removeAllDelta(keys: Seq[K]): DeltaAddWinsMapLattice[K, Map[Dot, (V, LastWriterWins[Instant, String])]] = {
    val subDeltas = keys.map(DeltaAddWinsMap.deltaRemove(_, _state))
    val delta = subDeltas.reduce((left, right) =>
      Lattice[DeltaAddWinsLastWriterWinsMapLattice[K, V]].merge(left, right)
    )
    mutate(delta)
    delta
  }

  def values: Map[K, V] =
    _state.store.map { case (k, mvReg) =>
      k -> mvReg.values.maxBy(_._2)._1
    }

  def merge(other: DeltaAddWinsLastWriterWinsMapLattice[K, V]): Unit = {
    mutate(other)
  }

  private def mutate(delta: DeltaAddWinsLastWriterWinsMapLattice[K, V]): Unit = {
    _deltas.append(delta)
    _state = Lattice[DeltaAddWinsLastWriterWinsMapLattice[K, V]].merge(_state, delta)
  }
}

object DeltaAddWinsLastWriterWinsMap {
  type DeltaAddWinsLastWriterWinsMapLattice[K, V] =
    DeltaAddWinsMapLattice[K, Map[Dot, (V, LastWriterWins[Instant, String])]]

  def empty[K, V]: DeltaAddWinsLastWriterWinsMapLattice[K, V] =
    DeltaAddWinsMap.empty

  implicit def timestampedValueLattice[V](using Ordering[LastWriterWins[Instant, String]]): Lattice[(V, LastWriterWins[Instant, String])] =
    (left, right) =>
      // note, this is incorrect when both are equal
      if left._2 <= right._2 then right else left

  type StateType[K, V] = DeltaAddWinsMapLattice[K, Map[Dot, (V, LastWriterWins[Instant, String])]]

  given deltaAddWinsMapLattice[K, V]: Lattice[StateType[K, V]] = WithContext.CausalWithDotMapLattice

}
