package com.github.ckuessner.encrdt.encrypted.statebased

import com.github.ckuessner.ardt.causality.VectorClock
import VectorClock.VectorClockOrdering
import com.github.ckuessner.encrdt.lattices.SemiLattice
import com.github.plokhotnyuk.jsoniter_scala.core.JsonValueCodec
import com.google.crypto.tink.Aead

abstract class UntrustedReplica(initialStates: Set[EncryptedState])(using
    tJsonCodec: JsonValueCodec[EncryptedState],
    vcJsonCodec: JsonValueCodec[VectorClock]
) extends Replica {

  protected var stateStore: Set[EncryptedState] = initialStates

  protected var versionVector: VectorClock = scala.compiletime.uninitialized
  versionVector = {
    if (initialStates.isEmpty) VectorClock()
    else initialStates.map(_.versionVector).reduce((l, r) => l.merged(r))
  }

  def decrypt[T: SemiLattice](aead: Aead)(implicit tCodec: JsonValueCodec[T]): DecryptedState[T] = {
    stateStore
      .map(encState => encState.decrypt[T](aead))
      .reduce((l, r) => SemiLattice[DecryptedState[T]].merged(l, r))
  }

  def receive(newState: EncryptedState): Unit = {
    if (!VectorClockOrdering.lteq(newState.versionVector, versionVector)) {
      // Update VersionVector
      versionVector = versionVector.merged(newState.versionVector)
      // newState is actually new (i.e., contains new updates)
      disseminate(newState)
    } else {
      // received state may already be subsumed by some state in the stateStore
      if (stateStore.exists(oldState => VectorClockOrdering.lteq(newState.versionVector, oldState.versionVector))) {
        // The newState is already subsumed by a single state in the stateStore
        return
      }
    }

    stateStore = leastUpperBound(
      stateStore.filterNot(oldState =>
        VectorClockOrdering.lteq(oldState.versionVector, newState.versionVector)
      ) + newState
    )

    Console.println(stateStore.map(_.versionVector))
  }

  private def leastUpperBound(states: Set[EncryptedState]): Set[EncryptedState] = {
    if (states.size <= 1) return states

    inline def subsetIsUpperBound(subset: Set[(EncryptedState, Int)], state: EncryptedState): Boolean = {
      val mergeOfAllOtherVersionVectors = subset
        .map(_._1.versionVector)
        .foldLeft(VectorClock()) { case (a: VectorClock, b: VectorClock) => a.merged(b) }

      // Check if this state is subsumed by the merged state of all other values
      VectorClockOrdering.lteq(state.versionVector, mergeOfAllOtherVersionVectors)
    }

    val indexedStates = states.zipWithIndex

    def rec(subset: Set[(EncryptedState, Int)]): Set[EncryptedState] = {
      val removable = subset.filter { case (state, index) =>
        subsetIsUpperBound(subset.filterNot(_._2 == index), state)
      }.toList

      if (removable.isEmpty) return subset.map(_._1)

      // Optimization: Don't test removal of states with a lower index of the state that was removed by caller of rec()
      // Requires that removable is traversed according to natural order
      var optimalSubtree = subset.map(_._1)
      removable.foreach { case (state, i) =>
        val subsetWithoutState = subset.filter(_._2 == i)
        if (subsetIsUpperBound(subsetWithoutState, state)) {
          val result = rec(subsetWithoutState)
          if (result.size < optimalSubtree.size) optimalSubtree = result
        }
      }

      optimalSubtree
    }

    rec(indexedStates)
  }
}

object UntrustedReplica {
  given encStatePOrd(using vcJsonCodec: JsonValueCodec[VectorClock]): PartialOrdering[EncryptedState] with {
    override def tryCompare(x: EncryptedState, y: EncryptedState): Option[Int] =
      VectorClockOrdering.tryCompare(x.versionVector, y.versionVector)

    override def lteq(x: EncryptedState, y: EncryptedState): Boolean =
      VectorClockOrdering.lteq(x.versionVector, y.versionVector)
  }
}
