package de.ckuessner
package encrdt.actors

import encrdt.lattices.interfaces.SemiLattice

import akka.actor.typed.scaladsl.ActorContext

class ObservableSynchronizationAdapter[T: SemiLattice](val stateChangedHandler: () => Unit,
                                                       context: ActorContext[_],
                                                       replicaId: String,
                                                       initialState: T)
  extends SynchronizationAdapter[T](context, replicaId, initialState) {

  @inline
  private def runIfStateChanged(call: () => Unit, stateChangedHandler: () => Unit): Unit = {
    val stateBeforeCommand = super.state
    call()
    if (stateBeforeCommand != super.state) {
      stateChangedHandler()
    }
  }

  override def handleCommand(msg: SynchronizationAdapter.Command[T]): Unit = runIfStateChanged(
    () => super.handleCommand(msg),
    stateChangedHandler
  )

  override def notifyPeersAboutStateChange(newState: T): Unit = {
    stateChangedHandler()
    super.notifyPeersAboutStateChange(newState)
  }


}
