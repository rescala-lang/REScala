package rescala.events

import rescala.{DepHolder, Dependent, ReactiveEngine, Signal}

/** A wrapped event inside a signal, that gets "flattened" to a plain event node */
class WrappedEvent[T](wrapper: Signal[Event[T]]) extends EventNode[T] with Dependent {

  var currentValue: T = _

  updateDependencies()

  private def updateDependencies() = setDependOn(Set(wrapper, wrapper.get))

  def triggerReevaluation(): Unit = {
    notifyDependents(currentValue)
  }

  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    if(dep eq wrapper) {
	    updateDependencies()
    }
    else if(dep eq wrapper.get) {
      currentValue = change.asInstanceOf[T]
    	ReactiveEngine.addToEvalQueue(this)
    }
    else throw new IllegalStateException("Illegal DepHolder " + dep)

  }

}
