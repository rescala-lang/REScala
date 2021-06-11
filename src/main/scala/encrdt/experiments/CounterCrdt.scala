package de.ckuessner
package encrdt.experiments

/**
 * Counter CRDT using states
 */
class CounterCrdt(val replicaId: Int) extends Crdt {

  type StateT = CounterCrdtState

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
    state = state.merge(remoteState)
  }

  // Local counts
  def query(): Int = state.positiveCounts.values.sum - state.negativeCounts.values.sum

  def update(delta: Int): Unit = {
    state = state.updated(replicaId, delta)
  }
}

// Encapsulates the state of the CRDT
case class CounterCrdtState(positiveCounts: Map[Int, Int] = Map(),
                            negativeCounts: Map[Int, Int] = Map()) {

  def merge(other: CounterCrdtState): CounterCrdtState = CounterCrdtState(
    mergeCounterMaps(positiveCounts, other.positiveCounts),
    mergeCounterMaps(negativeCounts, other.negativeCounts)
  )

  def updated(replicaId: Int, delta: Int): CounterCrdtState = {
    if (delta > 0) this.copy(
      positiveCounts = positiveCounts.updatedWith(replicaId)(value => Some(value.getOrElse(0) + delta))
    )
    else if (delta < 0) this.copy(
      negativeCounts = negativeCounts.updatedWith(replicaId)(value => Some(value.getOrElse(0) + delta.abs))
    )
    else this
  }

  // Merges mappings of both maps, takes biggest value if key is present in both maps
  def mergeCounterMaps[K](a: Map[K, Int], b: Map[K, Int]): Map[K, Int] =
    (a.keySet ++ b.keySet)
      .map(key => key -> a.getOrElse(key, 0).max(b.getOrElse(key, 0)))
      .toMap
}
