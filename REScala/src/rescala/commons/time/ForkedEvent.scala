package rescala.commons.time

import rescala.events.EventNode
import rescala.Dependent
import rescala.DepHolder
import rescala.ReactiveEngine
import rescala.TS

class ForkedEvent[T] extends EventNode[T] with Dependent {
 
  var storedVal: Any = _
  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    storedVal = change
    ReactiveEngine.addToEvalQueue(this)
  }
  
  def triggerReevaluation() {
    timestamps += TS.newTs // Testing
    notifyDependents(storedVal)
  }

}

class ImperativeForkEvent[T](children: ForkedEvent[T]*) extends EventNode[T] with DepHolder {
  
  // add initial children as dependents
  for(c <- children) +=(c)
  
  def +=(e: ForkedEvent[T]) = addDependent(e)
  def -=(e: ForkedEvent[T]) = removeDependent(e)
  
  
  /* Trigger the event */
  def apply(v: T): Unit = {
    TS.nextRound()
    timestamps += TS.newTs
    notifyDependents(v)
    ReactiveEngine.startEvaluation()
  }
}
