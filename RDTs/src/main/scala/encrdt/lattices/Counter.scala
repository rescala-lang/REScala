package de.ckuessner
package encrdt.lattices

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

/**
 * Counter CRDT using states
 */
class Counter(val replicaId: Int) {

  def this(replicaId: Int, initialState: CounterCrdtLattice) {
    this(replicaId)
    this.state = initialState
  }

  // Local state of the CRDT
  private var _state = CounterCrdtLattice()

  def state: CounterCrdtLattice = _state

  private def state_=(state: CounterCrdtLattice): Unit = {
    _state = state
  }

  def merge(remoteState: CounterCrdtLattice): Unit = {
    state = SemiLattice.merged(state, remoteState)
  }

  // Local counts
  def query(): Int = state.query()

  def update(delta: Int): Unit = {
    state = state.updated(replicaId, delta)
  }
}


// Encapsulates the state of the CRDT
case class CounterCrdtLattice(positiveCounts: Map[Int, Int] = Map(),
                              negativeCounts: Map[Int, Int] = Map()) {

  def updated(replicaId: Int, delta: Int): CounterCrdtLattice = {
    if (delta > 0) this.copy(
      positiveCounts = positiveCounts.updatedWith(replicaId)(value => Some(value.getOrElse(0) + delta))
    )
    else if (delta < 0) this.copy(
      negativeCounts = negativeCounts.updatedWith(replicaId)(value => Some(value.getOrElse(0) + delta.abs))
    )
    else this
  }

  def query(): Int = positiveCounts.values.sum - negativeCounts.values.sum
}

object CounterCrdtLattice {
  implicit val jsonCodec: JsonValueCodec[CounterCrdtLattice] = JsonCodecMaker.make

  implicit val semiLattice: SemiLattice[CounterCrdtLattice] = (left: CounterCrdtLattice, right: CounterCrdtLattice) =>
    CounterCrdtLattice(
      mergeCounterMaps(left.positiveCounts, right.positiveCounts),
      mergeCounterMaps(left.negativeCounts, right.negativeCounts)
    )

  // Merges mappings of both maps, takes biggest value if key is present in both maps
  def mergeCounterMaps[K](a: Map[K, Int], b: Map[K, Int]): Map[K, Int] =
    (a.keySet ++ b.keySet)
      .map(key => key -> a.getOrElse(key, 0).max(b.getOrElse(key, 0)))
      .toMap
}
