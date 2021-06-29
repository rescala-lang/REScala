package de.ckuessner
package encrdt.experiments

import com.github.plokhotnyuk.jsoniter_scala.core._
import com.github.plokhotnyuk.jsoniter_scala.macros._

/**
 * Counter CRDT using states
 */
class CounterCrdt(val replicaId: Int) {

  def this(replicaId: Int, initialState: CounterCrdtState) {
    this(replicaId)
    this.state = initialState
  }

  // Local state of the CRDT
  private var _state = CounterCrdtState()

  def state: CounterCrdtState = _state

  private def state_=(state: CounterCrdtState): Unit = {
    _state = state
  }

  def merge(remoteState: CounterCrdtState): Unit = {
    state = SemiLattice.merged(state, remoteState)
  }

  // Local counts
  def query(): Int = state.query()

  def update(delta: Int): Unit = {
    state = state.updated(replicaId, delta)
  }
}


// Encapsulates the state of the CRDT
case class CounterCrdtState(positiveCounts: Map[Int, Int] = Map(),
                            negativeCounts: Map[Int, Int] = Map()) {

  def updated(replicaId: Int, delta: Int): CounterCrdtState = {
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

object CounterCrdtState {
  implicit val jsonCodec: JsonValueCodec[CounterCrdtState] = JsonCodecMaker.make

  implicit val semiLattice: SemiLattice[CounterCrdtState] = (left: CounterCrdtState, right: CounterCrdtState) =>
    CounterCrdtState(
      mergeCounterMaps(left.positiveCounts, right.positiveCounts),
      mergeCounterMaps(left.negativeCounts, right.negativeCounts)
    )

  // Merges mappings of both maps, takes biggest value if key is present in both maps
  def mergeCounterMaps[K](a: Map[K, Int], b: Map[K, Int]): Map[K, Int] =
    (a.keySet ++ b.keySet)
      .map(key => key -> a.getOrElse(key, 0).max(b.getOrElse(key, 0)))
      .toMap
}
