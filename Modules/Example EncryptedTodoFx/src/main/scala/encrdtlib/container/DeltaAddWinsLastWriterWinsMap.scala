package encrdtlib.container

import encrdtlib.container.DeltaAddWinsLastWriterWinsMap.{DeltaAddWinsLastWriterWinsMapLattice, deltaAddWinsMapLattice}
import encrdtlib.lattices.DeltaAddWinsMap.DeltaAddWinsMapLattice
import encrdtlib.lattices.DeltaMultiValueRegister.DeltaMultiValueRegisterLattice
import encrdtlib.lattices.{DeltaAddWinsMap, DeltaMultiValueRegister}
import kofre.base.{Bottom, Lattice, Uid}
import kofre.datatypes.alternatives.lww.GenericLastWriterWins
import kofre.dotted.{DotFun, DotMap, Dotted, DottedLattice}
import kofre.time.Dot

import java.time.Instant
import scala.collection.mutable.ArrayBuffer
import scala.math.Ordering.Implicits.infixOrderingOps

class DeltaAddWinsLastWriterWinsMap[K, V](
    val replicaId: Uid,
    initialState: DeltaAddWinsLastWriterWinsMapLattice[K, V] = DeltaAddWinsLastWriterWinsMap.empty[K, V],
    initialDeltas: Vector[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = Vector()
) {
  protected var _state: DeltaAddWinsLastWriterWinsMapLattice[K, V]               = initialState
  protected val _deltas: ArrayBuffer[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = ArrayBuffer.from(initialDeltas)

  def state: DeltaAddWinsLastWriterWinsMapLattice[K, V] = _state

  def deltas: Vector[DeltaAddWinsLastWriterWinsMapLattice[K, V]] =
    _deltas.toVector // TODO: Maybe change this to another type

  def get(key: K): Option[V] =
    _state.data.repr.get(key)
      .map(_.repr.values)
      .getOrElse(Nil)
      .maxByOption(_._2.timestamp)
      .map(_._1)

  def put(key: K, value: V): Unit =
    mutate(
      DeltaAddWinsMap.deltaMutate[K, DotFun[(
          V,
          GenericLastWriterWins[java.time.Instant, Uid]
      )]](
        key,
        DotFun.empty,
        delta =>
          DeltaMultiValueRegister.deltaWrite(
            (value, GenericLastWriterWins(Instant.now(), replicaId)),
            replicaId,
            delta
          ),
        _state
      )
    )

  def putDelta(key: K, value: V): DeltaAddWinsLastWriterWinsMapLattice[K, V] = {
    val delta: DeltaAddWinsLastWriterWinsMapLattice[K, V] =
      DeltaAddWinsMap.deltaMutate(
        key,
        DotFun.empty,
        m => DeltaMultiValueRegister.deltaWrite((value, GenericLastWriterWins(Instant.now(), replicaId)), replicaId, m),
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
    _state.data.repr.map { case (k, mvReg) =>
      k -> mvReg.repr.values.maxBy(_._2.timestamp)._1
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
    DeltaAddWinsMapLattice[K, DotFun[(V, GenericLastWriterWins[Instant, Uid])]]

  def empty[K, V]: DeltaAddWinsLastWriterWinsMapLattice[K, V] =
    DeltaAddWinsMap.empty

  type StateType[K, V] = DeltaAddWinsLastWriterWinsMapLattice[K, V]

  given deltaAddWinsMapLattice[K, V]: Lattice[DeltaAddWinsLastWriterWinsMapLattice[K, V]] = {
    val timestampedValueLattice: Lattice[(V, GenericLastWriterWins[Instant, Uid])] =
      (left, right) =>
        // note, this is incorrect when both are equal
        if left._2.timestamp <= right._2.timestamp then right
        else left
    given DottedLattice[DotFun[(V, GenericLastWriterWins[Instant, Uid])]] =
      DotFun.dottedLattice[(V, GenericLastWriterWins[Instant, Uid])](using timestampedValueLattice)
    DotMap.dottedLattice[K, DotFun[(V, GenericLastWriterWins[Instant, Uid])]]
  }

}
