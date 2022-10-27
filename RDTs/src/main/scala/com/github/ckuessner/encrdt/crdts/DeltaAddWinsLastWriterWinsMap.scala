package com.github.ckuessner.encrdt.crdts

import com.github.ckuessner.encrdt.crdts.interfaces.{Crdt, MapCrdt}
import com.github.ckuessner.encrdt.causality.DotStore.DotFun
import com.github.ckuessner.encrdt.lattices.SemiLattice
import DeltaAddWinsLastWriterWinsMap.{DeltaAddWinsLastWriterWinsMapLattice, timestampedValueLattice}
import DeltaAddWinsMap.DeltaAddWinsMapLattice
import com.github.ckuessner.encrdt.lattices.LastWriterWinsTagLattice.lwwLattice

import java.time.Instant
import scala.collection.mutable.ArrayBuffer

class DeltaAddWinsLastWriterWinsMap[K, V](val replicaId: String,
                                          initialState: DeltaAddWinsLastWriterWinsMapLattice[K, V] = DeltaAddWinsLastWriterWinsMap.bottom[K, V],
                                          initialDeltas: Vector[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = Vector()
                                         ) extends MapCrdt[K, V] with Crdt[DeltaAddWinsLastWriterWinsMapLattice[K, V]] {
  protected var _state: DeltaAddWinsLastWriterWinsMapLattice[K, V] = initialState
  protected val _deltas: ArrayBuffer[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = ArrayBuffer.from(initialDeltas)

  def state: DeltaAddWinsLastWriterWinsMapLattice[K, V] = _state

  def deltas: Vector[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = _deltas.toVector // TODO: Maybe change this to another type

  override def get(key: K): Option[V] =
    _state.dotStore
      .getOrElse(key, Set())
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

  def removeDelta(key: K): DeltaAddWinsMapLattice[K, DotFun[(V,  (Instant, String))]] = {
    val delta = DeltaAddWinsMap.deltaRemove(key, _state)
    mutate(delta)
    delta
  }

  def removeAllDelta(keys: Seq[K]): DeltaAddWinsMapLattice[K, DotFun[(V,  (Instant, String))]] = {
    val subDeltas = keys.map(DeltaAddWinsMap.deltaRemove(_, _state))
    val delta = subDeltas.reduce((left, right) =>
      SemiLattice[DeltaAddWinsLastWriterWinsMapLattice[K, V]].merged(left, right)
    )
    mutate(delta)
    delta
  }

  override def values: Map[K, V] =
    _state.dotStore.map { case (k, mvReg) =>
      k -> mvReg.values.maxBy(_._2)._1
    }

  def merge(other: DeltaAddWinsLastWriterWinsMapLattice[K, V]): Unit = {
    mutate(other)
  }

  private def mutate(delta: DeltaAddWinsLastWriterWinsMapLattice[K, V]): Unit = {
    _deltas.append(delta)
    _state = SemiLattice[DeltaAddWinsLastWriterWinsMapLattice[K, V]].merged(_state, delta)
  }
}

object DeltaAddWinsLastWriterWinsMap {
  type DeltaAddWinsLastWriterWinsMapLattice[K, V] =
    DeltaAddWinsMapLattice[K, DotFun[(V, (Instant, String))]]

  def bottom[K, V]: DeltaAddWinsLastWriterWinsMapLattice[K, V] = DeltaAddWinsMap.bottom[K, DotFun[(V, (Instant, String))]]

  implicit def timestampedValueLattice[V]: SemiLattice[(V, (Instant, String))] = (left, right) => {
    if (SemiLattice.merged(left._2, right._2) == left._2) left
    else right
  }

  type StateType[K, V] = DeltaAddWinsMapLattice[K, DotFun[(V, (Instant, String))]]
}
