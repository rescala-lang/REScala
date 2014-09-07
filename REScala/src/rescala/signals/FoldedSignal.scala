package rescala.signals

import rescala.events.Event
import rescala.{DepHolder, DependentSignalImplementation}

class FoldedSignal[+T, +E](e: Event[E], init: T, f: (T, E) => T)
  extends DependentSignalImplementation[T] {

  addDependOn(e)

  // The cached value of the last occurence of e
  private[this] var lastEvent: E = _

  override def initialValue(): T = init
  override def calculateNewValue(): T = f(get, lastEvent)

  override def dependsOnchanged(change: Any, dep: DepHolder) = {
    if (dep eq e) {
      lastEvent = change.asInstanceOf[E]
    } else {
      // this would be an implementation error
      throw new RuntimeException("Folded Signals can only depend on a single event node")
    }

    super.dependsOnchanged(change, dep)
  }

}
