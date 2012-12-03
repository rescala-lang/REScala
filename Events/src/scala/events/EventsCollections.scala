package scala.events

import scala.collection.mutable.ListBuffer

class EventNodeListExists[T, U](list: List[T], evf: T => Event[U]) extends EventNode[U] {

  /*
   * Reaction to the observed events
   */
  def onEvt (id: Int, v: U, reacts: ListBuffer[(() => Unit, Trace)]) {
    reactions(id, v, reacts)
  }

  /*
  * Register to the events of all list elements and the list changes
  */
  protected override def deploy {
    list.foreach(target => evf(target) += onEvt _)
  }

  /*
  * Unregister from the events of all list elements and the list changes
  */
  protected override def undeploy {
    list.foreach(target => evf(target) -= onEvt _)
  }

  override def toString = getClass.getName

}

// vim: set ts=2 sw=2 et:
