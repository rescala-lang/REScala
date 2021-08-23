package de.ckuessner
package encrdt.actors

import encrdt.lattices.interfaces.SemiLattice

import akka.actor.typed.ActorRef
import akka.actor.typed.scaladsl.ActorContext

object SynchronizationAdapter {
  trait Command[T]

  final case class StateChanged[T: SemiLattice](changedOnReplica: String, newState: T) extends Command[T]

  //private final case class ReplicaUnavailable[T: SemiLattice](replicaId: String, ref: ActorRef[SynchronizationAdapter.Command[T]]) extends Command[T]

  final case class RequestState[T: SemiLattice](replyTo: ActorRef[State[T]]) extends Command[T]

  final case class State[T: SemiLattice](state: T)

  final case class RequestPeers[T: SemiLattice](replyTo: ActorRef[PeerMap[T]]) extends Command[T]

  final case class PeerMap[T: SemiLattice](peers: Map[String, ActorRef[Command[T]]])

}

class SynchronizationAdapter[T: SemiLattice](val context: ActorContext[_],
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

  // TODO: add lifecycle monitoring: https://doc.akka.io/docs/akka/current/actors.html#deathwatch
  private var peerRefs: Map[String, ActorRef[Command[T]]] = Map.empty

  //peerRefs.foreach {
  //  case (rId, actorRef: ActorRef[Command[T]]) =>
  //    context.watchWith(actorRef, ReplicaUnavailable(rId, actorRef))
  //}

  def handleCommand(msg: Command[T]): Unit = msg match {
    case StateChanged(changedOnReplica, newState) =>
      this._state = SemiLattice.merged(this._state, newState)

    case RequestState(replyTo) =>
      replyTo ! State(state)

    case RequestPeers(replyTo) =>
      replyTo ! PeerMap(peerRefs)
  }

  protected def notifyPeersAboutStateChange(newState: T): Unit = {
    // TODO: Failure semantics? https://doc.akka.io/docs/akka/current/general/message-delivery-reliability.html
    // TODO: Move to child actor
    peerRefs.filter {
      case (str, _) => str != replicaId
    } foreach {
      case (_, ref) => ref ! StateChanged(replicaId, newState)
    }
  }
}
