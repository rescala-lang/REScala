package de.ckuessner
package counter.actors

import encrdt.lattices.SemiLattice

import akka.actor.typed.ActorRef
import akka.actor.typed.receptionist.ServiceKey
import akka.actor.typed.scaladsl.ActorContext

class ObservableSynchronizationAdapter[T: SemiLattice](val stateChangedHandler: () => Unit,
                                                       context: ActorContext[_],
                                                       serviceKey: ServiceKey[SynchronizationAdapter.Command[T]],
                                                       replicaId: String,
                                                       initialState: T)
  extends SynchronizationAdapter[T](context, serviceKey, replicaId, initialState) {

  @inline
  private def runIfStateChanged(call: () => Unit, stateChangedHandler: () => Unit): Unit = {
    val stateBeforeCommand = super.state
    call()
    if (stateBeforeCommand != super.state) {
      stateChangedHandler()
    }
  }

  override def handleCommand(msg: SynchronizationAdapter.Command[T],
                             syncRef: ActorRef[SynchronizationAdapter.Command[T]]): Unit =
    runIfStateChanged(
      () => super.handleCommand(msg, syncRef),
      stateChangedHandler
    )

  override def notifyPeersAboutStateChange(newState: T): Unit = {
    stateChangedHandler()
    super.notifyPeersAboutStateChange(newState)
  }


}
