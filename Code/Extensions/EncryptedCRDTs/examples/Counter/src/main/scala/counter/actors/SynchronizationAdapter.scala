package de.ckuessner
package counter.actors

import encrdt.lattices.SemiLattice

import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.ServiceKey
import akka.actor.typed.scaladsl.ActorContext

object SynchronizationAdapter {
  trait Command[T]

  final case class StateChanged[T: SemiLattice](changedOnReplica: String, newState: T) extends Command[T]

  final case class RequestState[T: SemiLattice](replyTo: ActorRef[State[T]]) extends Command[T]

  final case class State[T: SemiLattice](state: T)

  final case class PeersChanged[T: SemiLattice](peers: Set[ActorRef[Command[T]]]) extends Command[T]
}

class SynchronizationAdapter[T: SemiLattice](val context: ActorContext[_],
                                             val serviceKey: ServiceKey[SynchronizationAdapter.Command[T]],
                                             val replicaId: String,
                                             initialState: T) {

  import SynchronizationAdapter._

  private var _state: T = initialState

  def state: T = _state

  def state_=(newState: T): Unit = {
    if (_state != newState) {
      notifyPeersAboutStateChange(newState)
    }
    _state = newState
  }

  private var peers: Set[ActorRef[Command[T]]] = Set.empty

  def handleCommand(msg: Command[T], syncRef: ActorRef[SynchronizationAdapter.Command[T]]): Unit = msg match {
    case StateChanged(changedOnReplica, newState) =>
      if (replicaId != changedOnReplica)
        this._state = SemiLattice.merged(this._state, newState)

    case RequestState(replyTo) =>
      replyTo ! State(state)

    case PeersChanged(listing) => peers = listing
  }

  protected def notifyPeersAboutStateChange(newState: T): Unit = {
    peers.foreach {
      ref => ref ! StateChanged(replicaId, newState)
    }
  }
}
