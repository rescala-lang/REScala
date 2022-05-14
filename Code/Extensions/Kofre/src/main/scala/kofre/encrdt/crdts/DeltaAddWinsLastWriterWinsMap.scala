package kofre.encrdt.crdts

import de.ckuessner.encrdt.crdts.interfaces.{Crdt, MapCrdt}
import kofre.encrdt.crdts.DeltaAddWinsLastWriterWinsMap.{DeltaAddWinsLastWriterWinsMapLattice, timestampedValueLattice}
import kofre.encrdt.crdts.DeltaAddWinsMap.DeltaAddWinsMapLattice
import kofre.base.{Bottom, Lattice}
import kofre.contextual.{ContextDecompose, ContextLattice, WithContext}
import kofre.time.Dot
import kofre.dotted.{DotFun, DotMap}
import kofre.primitives.LastWriterWins
import kofre.encrdt.crdts.DeltaAddWinsLastWriterWinsMap.deltaAddWinsMapLattice

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
    _state.store.get(key)
      .map(_.repr.values)
      .getOrElse(Nil)
      .maxByOption(_._2)
      .map(_._1)

  def put(key: K, value: V): Unit =
    mutate(
      DeltaAddWinsMap.deltaMutate(
        key,
        DotFun.empty,
        DeltaMultiValueRegister.deltaWrite((value, LastWriterWins(Instant.now(), replicaId)), replicaId, _),
        _state
      )
    )

  def putDelta(key: K, value: V): DeltaAddWinsLastWriterWinsMapLattice[K, V] = {
    val delta: DeltaAddWinsLastWriterWinsMapLattice[K, V] =
      DeltaAddWinsMap.deltaMutate(
        key,
        DotFun.empty,
        m => DeltaMultiValueRegister.deltaWrite((value, LastWriterWins(Instant.now(), replicaId)), replicaId, m),
        _state
      )
    mutate(delta)
    delta
  }

  def remove(key: K): Unit =
    mutate(DeltaAddWinsMap.deltaRemove(key, _state))

  def removeDelta(key: K): DeltaAddWinsLastWriterWinsMapLattice[K, V] = {
    val delta = DeltaAddWinsMap.deltaRemove(key, _state)
    mutate(delta)
    delta
  }

  def removeAllDelta(keys: Seq[K]): DeltaAddWinsLastWriterWinsMapLattice[K, V] = {
    val subDeltas = keys.map(DeltaAddWinsMap.deltaRemove(_, _state))
    val delta = subDeltas.reduce((left, right) =>
      DeltaAddWinsLastWriterWinsMap.deltaAddWinsMapLattice[K, V].merge(left, right)
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
    DeltaAddWinsMapLattice[K, DotFun[(V, LastWriterWins[Instant, String])]]

  def empty[K, V]: DeltaAddWinsLastWriterWinsMapLattice[K, V] =
    DeltaAddWinsMap.empty

  implicit def timestampedValueLattice[V](using
      Ordering[LastWriterWins[Instant, String]]
  ): Lattice[(V, LastWriterWins[Instant, String])] =
    (left, right) =>
      // note, this is incorrect when both are equal
      if left._2 <= right._2 then right else left

  type StateType[K, V] = DeltaAddWinsLastWriterWinsMapLattice[K, V]

  given deltaAddWinsMapLattice[K, V]: Lattice[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = {
    DotMap.contextLattice
  }

}
