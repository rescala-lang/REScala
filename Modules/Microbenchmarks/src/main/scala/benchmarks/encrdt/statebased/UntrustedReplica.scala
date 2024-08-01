package benchmarks.encrdt.statebased

import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import rdts.base.Lattice
import rdts.time.VectorClock
import replication.Aead

abstract class UntrustedReplica(initialStates: Set[EncryptedState]) extends Replica {
  protected var stateStore: Set[EncryptedState] = initialStates

  protected var versionVector: VectorClock = scala.compiletime.uninitialized
  versionVector = {
    if initialStates.isEmpty then VectorClock.zero
    else initialStates.map(_.versionVector).reduce((l, r) => l.merge(r))
  }

  def decrypt[T: Lattice](aead: Aead)(using tCodec: JsonValueCodec[T]): DecryptedState[T] = {
    stateStore
      .map(encState => encState.decrypt[T](aead))
      .reduce((l, r) => Lattice[DecryptedState[T]].merge(l, r))
  }

  def receive(newState: EncryptedState): Unit = {
    if !VectorClock.vectorClockOrdering.lteq(newState.versionVector, versionVector) then {
      // Update VersionVector
      versionVector = versionVector.merge(newState.versionVector)
      // newState is actually new (i.e., contains new updates)
      disseminate(newState)
    } else {
      // received state may already be subsumed by some state in the stateStore
      if
        stateStore.exists(oldState =>
          VectorClock.vectorClockOrdering.lteq(newState.versionVector, oldState.versionVector)
        )
      then {
        // The newState is already subsumed by a single state in the stateStore
        return
      }
    }

    stateStore = leastUpperBound(
      stateStore.filterNot(oldState =>
        VectorClock.vectorClockOrdering.lteq(oldState.versionVector, newState.versionVector)
      ) + newState
    )

    Console.println(stateStore.map(_.versionVector))
  }

  private def leastUpperBound(states: Set[EncryptedState]): Set[EncryptedState] = {
    if states.size <= 1 then return states

    @inline
    def subsetIsUpperBound(subset: Set[(EncryptedState, Int)], state: EncryptedState): Boolean = {
      val mergeOfAllOtherVersionVectors = subset
        .map(_._1.versionVector)
        .foldLeft(VectorClock.zero) { case (a: VectorClock, b: VectorClock) => a.merge(b) }

      // Check if this state is subsumed by the merge state of all other values
      VectorClock.vectorClockOrdering.lteq(state.versionVector, mergeOfAllOtherVersionVectors)
    }

    val indexedStates = states.zipWithIndex

    def rec(subset: Set[(EncryptedState, Int)]): Set[EncryptedState] = {
      val removable = subset.filter { case (state, index) =>
        subsetIsUpperBound(subset.filterNot(_._2 == index), state)
      }.toList

      if removable.isEmpty then return subset.map(_._1)

      // Optimization: Don't test removal of states with a lower index of the state that was removed by caller of rec()
      // Requires that removable is traversed according to natural order
      var optimalSubtree = subset.map(_._1)
      removable.foreach { case (state, i) =>
        val subsetWithoutState = subset.filter(_._2 == i)
        if subsetIsUpperBound(subsetWithoutState, state) then {
          val result = rec(subsetWithoutState)
          if result.size < optimalSubtree.size then optimalSubtree = result
        }
      }

      optimalSubtree
    }

    rec(indexedStates)
  }
}

object UntrustedReplica {
  object encStatePOrd extends PartialOrdering[EncryptedState] {
    override def tryCompare(x: EncryptedState, y: EncryptedState): Option[Int] =
      VectorClock.vectorClockOrdering.tryCompare(x.versionVector, y.versionVector)

    override def lteq(x: EncryptedState, y: EncryptedState): Boolean =
      VectorClock.vectorClockOrdering.lteq(x.versionVector, y.versionVector)
  }
}
